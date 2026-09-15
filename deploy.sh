#!/usr/bin/env bash
# ==============================================================================
# ont-process-app Deployment Script
# Description: Automated setup script for fresh Ubuntu/Debian server
# Requirements handled:
#   - System packages & build dependencies
#   - Java JRE + Nextflow installation
#   - R environment & renv package restoration
#   - Directory structure
#   - credentials.rds generation (for shinymanager)
# ==============================================================================

set -euo pipefail

# --- Color Formatting ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

log_info()    { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }
log_warn()    { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error()   { echo -e "${RED}[ERROR]${NC} $*"; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR="${SCRIPT_DIR}"
cd "${APP_DIR}"

# --- Default Parameters ---
APP_USER_DEFAULT="admin"
SHINY_PORT_DEFAULT=3838
APP_USER="${APP_USER:-}"
APP_PASS="${APP_PASS:-}"
SHINY_PORT="${SHINY_PORT:-$SHINY_PORT_DEFAULT}"
CURRENT_USER="${USER:-$(id -un 2>/dev/null || echo "root")}" 
SERVICE_USER="${SUDO_USER:-${CURRENT_USER}}"
SERVICE_GROUP="$(id -gn "${SERVICE_USER}" 2>/dev/null || echo "users")"
SKIP_SYS_DEPS=false
SKIP_SYSTEMD=false
NON_INTERACTIVE=false
RESET_CREDENTIALS=false

# --- Argument Parsing ---
print_help() {
    cat <<EOF
Usage: ./deploy.sh [OPTIONS]

Options:
  -u, --user <username>       Admin username for Shiny app login (default: ${APP_USER_DEFAULT})
  -p, --password <password>   Admin password for Shiny app login
  --port <port>               Port to run the Shiny app on (default: ${SHINY_PORT_DEFAULT})
  --skip-sys-deps             Skip apt-get system package installations
  --skip-systemd              Skip systemd service creation and registration
  --reset-credentials         Force overwrite of credentials.rds if it already exists
  -y, --non-interactive       Run without interactive prompts
  -h, --help                  Show this help message
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -u|--user)
            APP_USER="$2"
            shift 2
            ;;
        -p|--password)
            APP_PASS="$2"
            shift 2
            ;;
        --port)
            SHINY_PORT="$2"
            shift 2
            ;;
        --skip-sys-deps)
            SKIP_SYS_DEPS=true
            shift
            ;;
        --skip-systemd)
            SKIP_SYSTEMD=true
            shift
            ;;
        --reset-credentials)
            RESET_CREDENTIALS=true
            shift
            ;;
        -y|--non-interactive)
            NON_INTERACTIVE=true
            shift
            ;;
        -h|--help)
            print_help
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            print_help
            exit 1
            ;;
    esac
done

echo -e "${BOLD}${CYAN}===================================================${NC}"
echo -e "${BOLD}${CYAN}       Deploying ont-process-app on Fresh Server         ${NC}"
echo -e "${BOLD}${CYAN}===================================================${NC}"

# --- Helper: Command Detection with Sudo ---
SUDO=""
if [ "$(id -u)" -ne 0 ]; then
    if command -v sudo >/dev/null 2>&1; then
        SUDO="sudo"
    else
        log_warn "Not running as root and 'sudo' is not installed. System package installation might fail."
    fi
fi

# ==============================================================================
# 1. System Packages & Build Dependencies
# ==============================================================================
if [ "$SKIP_SYS_DEPS" = false ]; then
    if command -v apt-get >/dev/null 2>&1; then
        log_info "Detected Debian/Ubuntu system. Configuring CRAN apt repository..."
        export DEBIAN_FRONTEND=noninteractive
        $SUDO apt-get update -y
        $SUDO apt-get install -y dirmngr gnupg apt-transport-https ca-certificates software-properties-common wget curl

        # Add official CRAN repository to install latest R (R 4.4+)
        CODENAME="$(grep -oP '(?<=VERSION_CODENAME=).*' /etc/os-release 2>/dev/null | tr -d '"' || echo '')"
        if [ -n "$CODENAME" ]; then
            $SUDO wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | $SUDO gpg --dearmor --yes -o /etc/apt/trusted.gpg.d/cran_ubuntu.gpg 2>/dev/null || \
            $SUDO gpg --no-default-keyring --keyring /etc/apt/trusted.gpg.d/cran_ubuntu.gpg --keyserver keyserver.ubuntu.com --recv-keys 51716619E084DAB9 2>/dev/null || true
            $SUDO add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu ${CODENAME}-cran40/" 2>/dev/null || true
            $SUDO apt-get update -y
        fi

        log_info "Installing system packages & latest R..."
        
        # Check if Docker is already installed
        if command -v docker >/dev/null 2>&1; then
            log_info "Docker already installed. Skipping Docker installation."
        else
            log_info "Installing Docker..."
            $SUDO apt-get install -y docker.io
            log_success "Docker installed successfully."
        fi
        
        $SUDO apt-get install -y \
            curl \
            wget \
            git \
            tmux \
            tar \
            gzip \
            unzip \
            build-essential \
            ca-certificates \
            gnupg \
            dirmngr \
            software-properties-common \
            openjdk-17-jre-headless \
            r-base \
            r-base-dev \
            libcurl4-openssl-dev \
            libssl-dev \
            libxml2-dev \
            libfontconfig1-dev \
            libharfbuzz-dev \
            libfribidi-dev \
            libfreetype6-dev \
            libpng-dev \
            libtiff5-dev \
            libjpeg-dev \
            libgit2-dev \
            libuv1-dev \
            cmake
        log_success "System packages installed successfully."
    else
        log_warn "apt-get not detected. Please ensure R, Java 11+, tmux, curl, and build tools are installed."
    fi
else
    log_info "Skipping system dependencies (--skip-sys-deps)."
fi

# ==============================================================================
# 2. Nextflow Installation (Pinned Version: 25.04.7)
# ==============================================================================
NEXTFLOW_VERSION="${NEXTFLOW_VERSION:-25.04.7}"
log_info "Checking Nextflow (target version: v${NEXTFLOW_VERSION})..."

install_nextflow() {
    local target_dir="$1"
    log_info "Installing Nextflow v${NEXTFLOW_VERSION} into ${target_dir}..."
    local temp_nxf="/tmp/nextflow_$$"
    if curl -fsSL "https://github.com/nextflow-io/nextflow/releases/download/v${NEXTFLOW_VERSION}/nextflow" -o "$temp_nxf"; then
        chmod +x "$temp_nxf"
        if [ -w "$target_dir" ]; then
            mv "$temp_nxf" "${target_dir}/nextflow"
        elif [ -n "$SUDO" ]; then
            $SUDO mv "$temp_nxf" "${target_dir}/nextflow"
        else
            mkdir -p "$HOME/.local/bin"
            mv "$temp_nxf" "$HOME/.local/bin/nextflow"
            export PATH="$HOME/.local/bin:$PATH"
        fi
    else
        log_warn "Direct release download failed. Retrying with NXF_VER=${NEXTFLOW_VERSION} get.nextflow.io..."
        cd /tmp
        NXF_VER="$NEXTFLOW_VERSION" curl -fsSL https://get.nextflow.io | bash
        if [ -w "$target_dir" ]; then
            mv nextflow "${target_dir}/nextflow"
        elif [ -n "$SUDO" ]; then
            $SUDO mv nextflow "${target_dir}/nextflow"
        else
            mkdir -p "$HOME/.local/bin"
            mv nextflow "$HOME/.local/bin/nextflow"
            export PATH="$HOME/.local/bin:$PATH"
        fi
        cd "${APP_DIR}"
    fi
}

if ! command -v nextflow >/dev/null 2>&1; then
    install_nextflow "/usr/local/bin"
fi

if command -v nextflow >/dev/null 2>&1; then
    NXF_INSTALLED_VERSION=$(nextflow -version | grep -oP '(?<=version\s)\S+')
    log_success "Nextflow v${NXF_INSTALLED_VERSION} is installed."
else
    log_warn "Nextflow installation skipped or failed. Manual installation may be needed."
fi

# ==============================================================================
# 3. Create Required Directories
# ==============================================================================
log_info "Setting up application directories..."
mkdir -p "${APP_DIR}/logs"
mkdir -p "${APP_DIR}/data"
if [ -n "$SUDO" ]; then
    $SUDO chown -R "${SERVICE_USER}:${SERVICE_GROUP}" "${APP_DIR}/logs" "${APP_DIR}/data"
fi
log_success "Directories created: logs, data"

# ==============================================================================
# Setup credentials.rds
# ==============================================================================
log_info "Configuring credentials.rds..."
if [ -f "credentials.rds" ] && [ "$RESET_CREDENTIALS" = false ]; then
    log_info "credentials.rds already exists. Skipping (use --reset-credentials to overwrite)."
else
    if [ -z "$APP_USER" ]; then
        if [ "$NON_INTERACTIVE" = true ]; then
            APP_USER="$APP_USER_DEFAULT"
        else
            read -rp "Enter admin username for Shiny login [${APP_USER_DEFAULT}]: " input_user
            APP_USER="${input_user:-$APP_USER_DEFAULT}"
        fi
    fi

    if [ -z "$APP_PASS" ]; then
        if [ "$NON_INTERACTIVE" = true ]; then
            APP_PASS="$(tr -dc 'A-Za-z0-9' </dev/urandom | head -c 16 || openssl rand -base64 12)"
            log_warn "Generated random admin password: ${APP_PASS}"
        else
            while [ -z "$APP_PASS" ]; do
                read -rsp "Enter admin password for Shiny login: " input_pass
                echo ""
                if [ -z "$input_pass" ]; then
                    log_warn "Password cannot be empty. Please enter a valid password."
                else
                    APP_PASS="$input_pass"
                fi
            done
        fi
    fi

    log_info "Generating credentials.rds for user '${APP_USER}'..."
    Rscript -e "
      args <- commandArgs(trailingOnly = TRUE)
      user <- args[1]
      pass <- args[2]
      credentials <- data.frame(
        user = user,
        password = pass,
        admin = TRUE,
        comment = '',
        stringsAsFactors = FALSE
      )
      saveRDS(credentials, 'credentials.rds')
    " "$APP_USER" "$APP_PASS"

    chmod 600 credentials.rds
    if [ -n "$SUDO" ]; then
        $SUDO chown "${SERVICE_USER}:${SERVICE_GROUP}" credentials.rds
    fi
    log_success "credentials.rds generated successfully."
fi

# ==============================================================================
# 4. Docker/Containerd Setup
# ==============================================================================
if command -v docker >/dev/null 2>&1; then
    log_info "Docker detected. Verifying daemon..."
    if $SUDO systemctl is-active --quiet docker 2>/dev/null; then
        log_success "Docker daemon is running."
    else
        log_info "Starting Docker daemon..."
        $SUDO systemctl start docker 2>/dev/null || true
        $SUDO systemctl enable docker 2>/dev/null || true
    fi
    
    # Add current user to docker group (if not root)
    if [ "$(id -u)" -ne 0 ] && command -v docker >/dev/null 2>&1; then
        if ! groups "${SERVICE_USER:-$CURRENT_USER}" | grep -q docker; then
            log_info "Adding ${SERVICE_USER:-$CURRENT_USER} to docker group..."
            $SUDO usermod -aG docker "${SERVICE_USER:-$CURRENT_USER}" 2>/dev/null || true
            log_warn "User added to docker group. Log out and back in for changes to take effect."
        fi
    fi
else
    log_warn "Docker not installed. Nextflow container execution will be unavailable."
fi

# ==============================================================================
# 5. R Package Installation
# ==============================================================================
log_info "Installing R packages via renv..."
Rscript -e '
  get_binary_repo <- function() {
    codename <- ""
    if (file.exists("/etc/os-release")) {
      lines <- readLines("/etc/os-release")
      v_match <- grep("^VERSION_CODENAME=", lines, value = TRUE)
      u_match <- grep("^UBUNTU_CODENAME=", lines, value = TRUE)
      if (length(v_match) > 0) {
        codename <- sub("^VERSION_CODENAME=", "", v_match[1])
      } else if (length(u_match) > 0) {
        codename <- sub("^UBUNTU_CODENAME=", "", u_match[1])
      }
    }
    codename <- gsub("\"", "", trimws(codename))
    if (nchar(codename) > 0) {
      paste0("https://packagemanager.posit.co/cran/__linux__/", codename, "/latest")
    } else {
      "https://packagemanager.posit.co/cran/latest"
    }
  }

  binary_repo <- get_binary_repo()
  options(repos = c(CRAN = binary_repo))
  options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
  options(renv.config.rspm.enabled = TRUE)

  source_repo <- "https://packagemanager.posit.co/cran/latest"

  if (!requireNamespace("renv", quietly = TRUE)) {
    message("Installing renv package...")
    tryCatch({
      install.packages("renv", repos = binary_repo)
    }, error = function(e) {
      message("Binary install for renv failed, falling back to source: ", e$message)
      install.packages("renv", repos = source_repo)
    })
  }
  
  if (file.exists("renv.lock")) {
    message("Restoring environment from renv.lock (attempting pre-built binaries from ", binary_repo, ")...")
    res <- tryCatch({
      renv::restore(repos = c(CRAN = binary_repo), prompt = FALSE, retry = TRUE)
      TRUE
    }, error = function(e) {
      message("Binary restore failed: ", e$message)
      message("Falling back to building packages from source (", source_repo, ")...")
      FALSE
    })
    
    if (!res) {
      options(repos = c(CRAN = source_repo))
      renv::restore(repos = c(CRAN = source_repo), prompt = FALSE, retry = TRUE)
    }
  }
  
  message("Checking and installing any missing core packages directly...")
  pkgs <- c("shiny", "jsonlite", "bslib", "bsicons", "stringr", 
            "dplyr", "shinyvalidate", "shinyFiles", "shinyjs", 
            "processx", "digest", "hover", "reactable", "prettyunits", 
            "shinymanager", "fs")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing missing package: ", pkg)
      tryCatch({
        install.packages(pkg, repos = binary_repo)
      }, error = function(e) {
        message("Binary install for ", pkg, " failed, falling back to source...")
        install.packages(pkg, repos = source_repo)
      })
    }
  }
'

log_info "Verifying required R packages..."
Rscript -e '
  pkgs <- c("shiny", "jsonlite", "bslib", "bsicons", "stringr", 
            "dplyr", "shinyvalidate", "shinyFiles", "shinyjs", 
            "processx", "digest", "hover", "reactable", "prettyunits", 
            "shinymanager", "fs")
  missing <- c()
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing <- c(missing, pkg)
    }
  }
  if (length(missing) > 0) {
    stop(paste("Missing required packages:", paste(missing, collapse = ", ")))
  }
'
log_success "All required R packages are installed and verified."

# ==============================================================================
# 7. Setup Systemd Services
# ==============================================================================
if [ "$SKIP_SYSTEMD" = false ] && command -v systemctl >/dev/null 2>&1; then
    log_info "Setting up systemd service for ont-process-app..."

    R_BIN="$(command -v R || echo "/usr/bin/R")"
    SERVICE_HOME="$(getent passwd "${SERVICE_USER}" 2>/dev/null | cut -d: -f6 || echo "/home/${SERVICE_USER}")"

    # --- Shiny App Service ---
    SHINY_SERVICE_FILE="/etc/systemd/system/ont-process-app.service"
    cat <<EOF | $SUDO tee "$SHINY_SERVICE_FILE" >/dev/null
[Unit]
Description=ONT Process App
After=network.target

[Service]
Type=simple
User=${SERVICE_USER}
Group=${SERVICE_GROUP}
WorkingDirectory=${APP_DIR}
Environment="PATH=/usr/local/bin:/usr/bin:/bin:${SERVICE_HOME}/.local/bin"
Environment="HOME=${SERVICE_HOME}"
ExecStart=${R_BIN} -e "shiny::runApp(appDir='${APP_DIR}', host='0.0.0.0', port=${SHINY_PORT})"
Restart=always
RestartSec=5
StandardOutput=append:${APP_DIR}/logs/shiny-app.log
StandardError=append:${APP_DIR}/logs/shiny-app.log

[Install]
WantedBy=multi-user.target
EOF
    log_success "Created systemd service: ${SHINY_SERVICE_FILE}"

    # Reload systemd and enable/start service
    $SUDO systemctl daemon-reload
    $SUDO systemctl enable --now ont-process-app.service

    log_success "Enabled and started ont-process-app.service."
else
    if [ "$SKIP_SYSTEMD" = true ]; then
        log_info "Skipping systemd service setup (--skip-systemd)."
    else
        log_warn "systemctl not available; skipping systemd service registration."
    fi
fi

# ==============================================================================
# 8. Firewall Configuration & Port Verification
# ==============================================================================
log_info "Checking firewall and opening port (${SHINY_PORT})..."

# Check UFW
if command -v ufw >/dev/null 2>&1; then
    if $SUDO ufw status | grep -qw "active"; then
        log_info "UFW firewall is active. Opening port ${SHINY_PORT}/tcp..."
        $SUDO ufw allow "${SHINY_PORT}/tcp" comment "ONT Process App" >/dev/null 2>&1 || true
        log_success "UFW firewall rule updated for port ${SHINY_PORT}."
    else
        log_info "UFW is inactive. Port ${SHINY_PORT} is unrestricted by UFW."
    fi
fi

# Check firewalld
if command -v firewall-cmd >/dev/null 2>&1 && systemctl is-active --quiet firewalld 2>/dev/null; then
    log_info "firewalld is active. Opening port ${SHINY_PORT}/tcp..."
    $SUDO firewall-cmd --permanent --add-port="${SHINY_PORT}/tcp" >/dev/null 2>&1 || true
    $SUDO firewall-cmd --reload >/dev/null 2>&1 || true
    log_success "firewalld rules updated."
fi

# ==============================================================================
# 9. Service Health Check & Status Report
# ==============================================================================
if [ "$SKIP_SYSTEMD" = false ] && command -v systemctl >/dev/null 2>&1; then
    log_info "Verifying service status and health..."
    sleep 3

    # Check Shiny App
    SHINY_STATUS="$(systemctl is-active ont-process-app.service 2>/dev/null || echo 'inactive')"
    if [ "$SHINY_STATUS" = "active" ]; then
        log_success "ont-process-app.service is active (running)."
    else
        log_error "ont-process-app.service failed to start (status: ${SHINY_STATUS}). Recent logs:"
        $SUDO journalctl -u ont-process-app.service -n 20 --no-pager || true
    fi

    # Test HTTP endpoint connectivity locally
    log_info "Testing local HTTP endpoint..."
    SHINY_HTTP_CODE="$(curl -s -o /dev/null -w "%{http_code}" --connect-timeout 5 "http://127.0.0.1:${SHINY_PORT}" 2>/dev/null || echo "000")"

    if [ "$SHINY_HTTP_CODE" != "000" ]; then
        log_success "Shiny App HTTP response: ${SHINY_HTTP_CODE} OK (http://127.0.0.1:${SHINY_PORT})"
    else
        log_warn "Shiny App not responding on http://127.0.0.1:${SHINY_PORT} yet. It may still be initializing R packages."
    fi
fi

# ==============================================================================
# 10. Deployment Summary & Usage Instructions
# ==============================================================================
IP_ADDR="$(ip route get 1.1.1.1 2>/dev/null | awk '{print $7}')"


echo ""
echo -e "${BOLD}${GREEN}===================================================${NC}"
echo -e "${BOLD}${GREEN}          Deployment Setup Completed!              ${NC}"
echo -e "${BOLD}${GREEN}===================================================${NC}"
echo ""
echo -e "${BOLD}Access URL:${NC}"
echo -e "   Shiny App: ${CYAN}http://${IP_ADDR}:${SHINY_PORT}${NC}"
echo ""
echo -e "${BOLD}Service Status Summary:${NC}"
if command -v systemctl >/dev/null 2>&1 && [ "$SKIP_SYSTEMD" = false ]; then
    echo -e "   ont-process-app.service: $(systemctl is-active ont-process-app 2>/dev/null || echo 'inactive')"
fi
echo ""
echo -e "${BOLD}Useful Management Commands:${NC}"
echo -e "   Check status:  sudo systemctl status ont-process-app"
echo -e "   Restart app:   sudo systemctl restart ont-process-app"
echo -e "   Stop app:      sudo systemctl stop ont-process-app"
echo -e "   Remove app:    sudo systemctl disable --now ont-process-app && sudo rm /etc/systemd/system/ont-process-app.service && sudo systemctl daemon-reload"
echo -e "   View app logs: sudo journalctl -u ont-process-app -f (or tail -f logs/shiny-app.log)"
echo ""
echo -e "${BOLD}Note:${NC} Nextflow pipelines will run with Docker/Singularity as configured."
echo ""
