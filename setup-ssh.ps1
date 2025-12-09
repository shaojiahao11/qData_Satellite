# setup-ssh.ps1
# Automatically set up SSH key-based authentication for passwordless login.
# Pure ASCII, compatible with Windows PowerShell and PowerShell Core.

param(
    [string]$ServerUser = "root",
    [string]$ServerHost = "101.33.200.2",
    [string]$SshKeyPath = "$env:USERPROFILE\.ssh\id_ed25519"
)

# 🔧 Fix path for OpenSSH on Windows (use forward slashes)
$SshKeyPath = $SshKeyPath.Replace('\', '/')
$PublicKeyPath = "$SshKeyPath.pub"

# Step 1: Check if local private key already exists
if (Test-Path -Path $SshKeyPath) {
    Write-Host "Local SSH private key found: $SshKeyPath" -ForegroundColor Green
} else {
    Write-Host "Generating new SSH key pair..." -ForegroundColor Yellow
    ssh-keygen -t ed25519 -f $SshKeyPath -N '""' -C "auto-setup-ssh"
    if ($LASTEXITCODE -ne 0) {
        Write-Host "ERROR: Failed to generate SSH key!" -ForegroundColor Red
        exit 1
    }
    Write-Host "SSH key generated: $SshKeyPath" -ForegroundColor Green
}

# Step 2: Ensure public key file exists
if (-not (Test-Path -Path $PublicKeyPath)) {
    Write-Host "ERROR: Public key file not found: $PublicKeyPath" -ForegroundColor Red
    exit 1
}

# Common SSH options for reliability
$SshOptions = @(
    "-i", "`"$SshKeyPath`"",
    "-o", "IdentitiesOnly=yes",
    "-o", "PubkeyAuthentication=yes",
    "-o", "PasswordAuthentication=no",
    "-o", "BatchMode=yes",
    "-o", "ConnectTimeout=10",
    "-o", "StrictHostKeyChecking=no"
)

# Step 3: Test if passwordless login already works
Write-Host "Testing passwordless SSH access..." -ForegroundColor Yellow
$testResult = & ssh @SshOptions "${ServerUser}@${ServerHost}" "echo 'AUTH_OK'" 2>$null

if ($LASTEXITCODE -eq 0 -and $testResult -eq "AUTH_OK") {
    Write-Host "SUCCESS: Already configured. No action needed." -ForegroundColor Green
    exit 0
}

# Step 4: Upload public key to server
Write-Host "Uploading public key to server..." -ForegroundColor Yellow
$TempPubKeyRemote = "/tmp/id_auto_setup.pub"
$scpResult = & scp @SshOptions $PublicKeyPath "${ServerUser}@${ServerHost}:${TempPubKeyRemote}"
if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: Failed to upload public key!" -ForegroundColor Red
    exit 1
}

# Step 5: Configure authorized_keys on remote server
Write-Host "Configuring SSH on remote server..." -ForegroundColor Magenta
$remoteCmd = "mkdir -p ~/.ssh && chmod 700 ~/.ssh && cat '$TempPubKeyRemote' >> ~/.ssh/authorized_keys && chmod 600 ~/.ssh/authorized_keys && rm -f '$TempPubKeyRemote' && echo 'SSH key installed.'"
& ssh @SshOptions "${ServerUser}@${ServerHost}" $remoteCmd
if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: Failed to configure server!" -ForegroundColor Red
    exit 1
}

# Step 6: Final verification
Write-Host "Verifying passwordless login..." -ForegroundColor Green
$finalTest = & ssh @SshOptions "${ServerUser}@${ServerHost}" "echo 'FINAL_OK'" 2>$null
if ($LASTEXITCODE -eq 0 -and $finalTest -eq "FINAL_OK") {
    Write-Host "SUCCESS: Passwordless SSH is now ready!" -ForegroundColor Cyan
    Write-Host "You can now run your deployment script without entering a password." -ForegroundColor Green
} else {
    Write-Host "WARNING: Setup completed but verification failed. Please test manually:" -ForegroundColor Yellow
    Write-Host "  ssh -i `"$SshKeyPath`" ${ServerUser}@${ServerHost}" -ForegroundColor Yellow
}