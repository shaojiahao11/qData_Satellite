# push-code.ps1
# Push local source code to Git remote (origin main)

Write-Host "Checking git status..." -ForegroundColor Yellow
git status

Write-Host "Adding all changes..." -ForegroundColor Yellow
git add .

Write-Host "Committing changes..." -ForegroundColor Yellow
git commit -m "Update source code"

Write-Host "Pushing to origin/main..." -ForegroundColor Yellow
git push origin main

if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: Git push failed!" -ForegroundColor Red
    exit 1
}

Write-Host "SUCCESS: Code pushed to Git!" -ForegroundColor Green