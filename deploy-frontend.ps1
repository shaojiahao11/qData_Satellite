# deploy-frontend.ps1
# Deploy frontend and backend to server: root@101.33.200.2

# ==================== Configuration ====================
$SERVER_USER = "root"
$SERVER_HOST = "101.33.200.2"
$REMOTE_BASE_PATH = "/www/wwwroot/qdata"
$FRONTEND_DIR_NAME = "qdata-frontend"
$BACKEND_REPO_PATH = "/www/wwwroot/qdata/qdata-backend"

# ==================== Start Deployment ====================
Write-Host "Starting deployment to $SERVER_HOST..." -ForegroundColor Cyan

# 1. Build frontend locally
Write-Host "Building frontend with 'npm run build'..." -ForegroundColor Yellow
npm run build
if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: Frontend build failed!" -ForegroundColor Red
    exit 1
}

if (-not (Test-Path -Path "dist")) {
    Write-Host "ERROR: 'dist' folder not found after build!" -ForegroundColor Red
    exit 1
}

# 2. Define remote paths
$REMOTE_FRONTEND_CURRENT = "${REMOTE_BASE_PATH}/${FRONTEND_DIR_NAME}"
$REMOTE_FRONTEND_NEW     = "${REMOTE_BASE_PATH}/${FRONTEND_DIR_NAME}-new"
$REMOTE_FRONTEND_OLD     = "${REMOTE_BASE_PATH}/${FRONTEND_DIR_NAME}-old"

# 3. Upload to temporary directory (try rsync, fallback to scp)
Write-Host "Uploading frontend to temporary directory..." -ForegroundColor Yellow

$RSYNC_CMD = "wsl rsync -avz --exclude='.git' ./dist/ ${SERVER_USER}@${SERVER_HOST}:${REMOTE_FRONTEND_NEW}/"
Invoke-Expression $RSYNC_CMD
if ($LASTEXITCODE -ne 0) {
    Write-Host "WARNING: rsync failed, trying scp..." -ForegroundColor DarkYellow
    $SCP_CMD = "scp -r ./dist/* ${SERVER_USER}@${SERVER_HOST}:${REMOTE_FRONTEND_NEW}/"
    Invoke-Expression $SCP_CMD
    if ($LASTEXITCODE -ne 0) {
        Write-Host "ERROR: File transfer failed!" -ForegroundColor Red
        exit 1
    }
}

# 4. Atomic switch on server
Write-Host "Performing atomic switch..." -ForegroundColor Green

$SWAP_AND_CLEANUP = @"
set -e
echo 'Switching frontend version...'
rm -rf '$REMOTE_FRONTEND_OLD' 2>/dev/null || true
if [ -d '$REMOTE_FRONTEND_CURRENT' ]; then
    mv '$REMOTE_FRONTEND_CURRENT' '$REMOTE_FRONTEND_OLD'
fi
mv '$REMOTE_FRONTEND_NEW' '$REMOTE_FRONTEND_CURRENT'
echo 'Frontend switch completed.'
"@

$SSH_SWAP_CMD = "ssh ${SERVER_USER}@${SERVER_HOST} `"$SWAP_AND_CLEANUP`""
Invoke-Expression $SSH_SWAP_CMD
if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: Atomic switch failed!" -ForegroundColor Red
    exit 1
}

# 5. Update and build backend
Write-Host "Updating and building backend..." -ForegroundColor Magenta

$BACKEND_UPDATE_SCRIPT = @"
set -e
cd '$BACKEND_REPO_PATH'
echo 'Fetching latest code...'
git fetch origin
git reset --hard origin/main
echo 'Building with Maven Wrapper...'
./mvnw clean package -DskipTests
echo 'Backend JAR built successfully.'
"@

$SSH_BACKEND_CMD = "ssh ${SERVER_USER}@${SERVER_HOST} `"$BACKEND_UPDATE_SCRIPT`""
Invoke-Expression $SSH_BACKEND_CMD
if ($LASTEXITCODE -ne 0) {
    Write-Host "WARNING: Backend build failed, but frontend is deployed." -ForegroundColor DarkYellow
} else {
    Write-Host "SUCCESS: Backend updated and built!" -ForegroundColor Green
}

# Optional: Restart service (uncomment if needed)
# Write-Host "Restarting qdata service..." -ForegroundColor Cyan
# ssh ${SERVER_USER}@${SERVER_HOST} "systemctl restart qdata"

Write-Host "Deployment completed!" -ForegroundColor Cyan