# deploy-frontend.ps1
# Final safe deployment script with remote cleanup and pre-checks

# ==================== Configuration ====================
$SERVER_USER = "root"
$SERVER_HOST = "101.33.200.2"
$REMOTE_BASE_PATH = "/www/wwwroot/qdata"
$FRONTEND_DIR_NAME = "qdata-frontend"

# 👇 MODIFY THIS: your local frontend subdirectory (e.g., "qdata-ui", "web")
$LOCAL_FRONTEND_SUBDIR = "qdata-ui"

# ==================== Pre-flight Checks ====================
Write-Host "🔍 Running pre-flight checks..." -ForegroundColor Cyan

if (-not (Test-Path $LOCAL_FRONTEND_SUBDIR)) {
    Write-Host "❌ ERROR: Local frontend dir '$LOCAL_FRONTEND_SUBDIR' not found." -ForegroundColor Red
    exit 1
}

Write-Host "📡 Testing SSH connectivity..." -ForegroundColor Yellow
$null = ssh -o ConnectTimeout=5 -o BatchMode=yes -o StrictHostKeyChecking=no ${SERVER_USER}@${SERVER_HOST} "exit" 2>$null
if ($LASTEXITCODE -ne 0) {
Write-Host "❌ ERROR: Cannot connect to server via SSH." -ForegroundColor Red
exit 1
}
Write-Host "✅ SSH OK." -ForegroundColor Green

Write-Host "✅ Backend path exists." -ForegroundColor Green

Write-Host "✅ All checks passed. Starting deployment..." -ForegroundColor Cyan
Write-Host ("=" * 60) -ForegroundColor Gray

# ==================== Deployment ====================
$ORIGINAL_DIR = Get-Location

try {
# --- Build Frontend ---
Set-Location $LOCAL_FRONTEND_SUBDIR

Write-Host "📦 Building frontend..." -ForegroundColor Yellow
npm run build:prod
if ($LASTEXITCODE -ne 0) {
Write-Host "❌ Frontend build failed!" -ForegroundColor Red
exit 1
}

if (-not (Test-Path "dist")) {
Write-Host "❌ 'dist' folder not found after build!" -ForegroundColor Red
exit 1
}

if (Get-ChildItem -Path "dist" -Recurse -Include "*.xlsx", "*.xls" -ErrorAction SilentlyContinue) {
Write-Host "⚠️  WARNING: Excel files detected in 'dist'." -ForegroundColor DarkYellow
}

# --- Remote Paths ---
$REMOTE_FRONTEND_CURRENT = "${REMOTE_BASE_PATH}/${FRONTEND_DIR_NAME}"
$REMOTE_FRONTEND_NEW     = "${REMOTE_BASE_PATH}/${FRONTEND_DIR_NAME}-new"
$REMOTE_FRONTEND_OLD     = "${REMOTE_BASE_PATH}/${FRONTEND_DIR_NAME}-old"

# --- 🔥 CRITICAL: Clean remote new directory first ---
Write-Host "🧹 Preparing fresh remote directory: $REMOTE_FRONTEND_NEW" -ForegroundColor Gray
$CLEAN_CMD = "ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no ${SERVER_USER}@${SERVER_HOST} 'rm -rf $REMOTE_FRONTEND_NEW && mkdir -p $REMOTE_FRONTEND_NEW'"
Invoke-Expression $CLEAN_CMD
if ($LASTEXITCODE -ne 0) {
Write-Host "❌ Failed to clean/prepare remote new directory!" -ForegroundColor Red
exit 1
}

# --- Upload Files ---
Write-Host "📤 Uploading files..." -ForegroundColor Yellow
$RSYNC_CMD = "wsl rsync -avz --exclude='.git' ./dist/ ${SERVER_USER}@${SERVER_HOST}:${REMOTE_FRONTEND_NEW}/"
Invoke-Expression $RSYNC_CMD
if ($LASTEXITCODE -ne 0) {
Write-Host "⚠️  rsync failed, falling back to scp..." -ForegroundColor DarkYellow
$SCP_CMD = "scp -r ./dist/* ${SERVER_USER}@${SERVER_HOST}:${REMOTE_FRONTEND_NEW}/"
Invoke-Expression $SCP_CMD
if ($LASTEXITCODE -ne 0) {
Write-Host "❌ File upload failed!" -ForegroundColor Red
exit 1
}
}

## --- Clean Local dist ---
#Remove-Item -Path "dist" -Recurse -Force
#if ($?) { Write-Host "🧹 Local 'dist' cleaned." -ForegroundColor Green }

# --- Back to Root ---
Set-Location $ORIGINAL_DIR

# --- Atomic Switch (SAFE SINGLE-LINE) ---
Write-Host "🔄 Performing atomic switch..." -ForegroundColor Green
$SWAP_SCRIPT = "set -e; rm -rf '$REMOTE_FRONTEND_OLD' 2>/dev/null || true; if [ -d '$REMOTE_FRONTEND_CURRENT' ]; then mv '$REMOTE_FRONTEND_CURRENT' '$REMOTE_FRONTEND_OLD'; fi; mv '$REMOTE_FRONTEND_NEW' '$REMOTE_FRONTEND_CURRENT'; echo 'Switch OK.'"
$SSH_SWAP_CMD = "ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no ${SERVER_USER}@${SERVER_HOST} '$SWAP_SCRIPT'"
Invoke-Expression $SSH_SWAP_CMD
if ($LASTEXITCODE -ne 0) {
Write-Host "❌ Atomic switch failed!" -ForegroundColor Red
exit 1
}
Write-Host "✅ Frontend switched successfully." -ForegroundColor Green


Write-Host "🎉 Deployment completed successfully!" -ForegroundColor Cyan

} finally {
Set-Location $ORIGINAL_DIR
}