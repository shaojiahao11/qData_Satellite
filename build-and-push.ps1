# build-and-push.ps1 (终极智能版)
# 支持多模块 Maven 项目 + 智能内存策略

$ProgressPreference = 'SilentlyContinue'
Write-Host "🚀 开始智能构建与推送..." -ForegroundColor Green

$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Definition
$FrontendDir = Join-Path $ProjectRoot "qdata-ui"
# 后端不再限定单个目录，而是检测整个 Java 代码库

$DistFrontend = Join-Path $ProjectRoot "dist-frontend"
$DistBackend = Join-Path $ProjectRoot "dist-backend"

# === 步骤 1: Git 暂存区检查 ===
Set-Location $ProjectRoot
git add .
git diff --cached --quiet
if ($LASTEXITCODE -eq 0) {
    Write-Host "ℹ️  暂存区无变更，跳过。" -ForegroundColor Yellow
    exit 0
}

# === 步骤 2: 智能检测变更 ===
$StagedFiles = git diff --cached --name-only

$FrontendChanged = $false
$BackendChanged = $false

foreach ($file in $StagedFiles) {
    # 前端：qdata-ui/ 下的源码或配置
    if ($file -like "qdata-ui/*") {
        if ($file -match '\.(vue|js|ts|jsx|tsx|css|scss|less|html)$' -or $file -like "qdata-ui/package*.json") {
            $FrontendChanged = $true
        }
    }
    # 后端：任何 .java, .xml, .yml, pom.xml（无论在哪）
    elseif ($file -match '\.(java|xml|yml|yaml|properties|sql)$' -or $file -like "*pom.xml") {
        $BackendChanged = $true
    }
}

if (-not $FrontendChanged -and -not $BackendChanged) {
    Write-Host "ℹ️  无前后端相关变更，跳过构建。" -ForegroundColor Yellow
}

# === 步骤 3: 构建前端（仅需时）===
if ($FrontendChanged) {
    Write-Host "📦 构建前端..." -ForegroundColor Cyan
    Set-Location $FrontendDir

    $LockFile = Join-Path $FrontendDir "package-lock.json"
    $NodeModules = Join-Path $FrontendDir "node_modules"

    if (-not (Test-Path $NodeModules) -or
            ((Test-Path $LockFile) -and (Get-Item $LockFile).LastWriteTime -gt (Get-Item $NodeModules).LastWriteTime)) {
        npm install --silent
        if ($LASTEXITCODE -ne 0) { exit 1 }
    }

    # ✅ 内存限制：仅当机器内存 ≤8GB 时启用（可选）
    # 如果你机器够强，直接用下面这行（推荐）：
    npm run build:prod
    # 如果你确实需要限制（比如 CI 环境），取消注释下一行：
    # $env:NODE_OPTIONS = "--max-old-space-size=1024"; npm run build:prod; $env:NODE_OPTIONS = $null

    if ($LASTEXITCODE -ne 0) { exit 1 }

    Remove-Item $DistFrontend -Recurse -Force -ErrorAction SilentlyContinue
    Copy-Item -Path (Join-Path $FrontendDir "dist") -Destination $DistFrontend -Recurse
    Write-Host "✅ 前端构建完成" -ForegroundColor Green
} else {
    Write-Host "⏭️  跳过前端构建" -ForegroundColor DarkGray
}

# === 步骤 4: 构建后端（仅需时）===
if ($BackendChanged) {
    Write-Host "⚙️  构建后端（多模块）..." -ForegroundColor Cyan
    Set-Location $ProjectRoot  # ⚠️ 关键：在根目录运行 mvn（多模块标准做法）

    # ✅ 不限制内存（除非你明确需要）
    mvn package -DskipTests --quiet
    # 如果你需要限制内存（低配机器），用：
    # $env:MAVEN_OPTS = "-Xmx1024m"; mvn package -DskipTests --quiet; $env:MAVEN_OPTS = $null

    if ($LASTEXITCODE -ne 0) { exit 1 }

    # 从 qdata-server/target 找 JAR（假设主应用在这里）
    $JarPath = Join-Path $ProjectRoot "qdata-server/target"
    $JarFile = Get-ChildItem $JarPath -Filter "*.jar" -Recurse | Where-Object { $_.Name -notlike "*sources.jar" -and $_.Name -notlike "*javadoc.jar" } | Select-Object -First 1

    if (-not $JarFile) { Write-Host "❌ 未找到主 JAR 文件"; exit 1 }

    New-Item -ItemType Directory -Path $DistBackend -Force | Out-Null
    Copy-Item $JarFile.FullName (Join-Path $DistBackend "qdata-server.jar")
    Write-Host "✅ 后端构建完成" -ForegroundColor Green
} else {
    Write-Host "⏭️  跳过后端构建" -ForegroundColor DarkGray
}

# === 步骤 5: 提交推送 ===
Set-Location $ProjectRoot
git diff --cached --quiet
if ($LASTEXITCODE -eq 0) { exit 0 }

git commit -m "feat: update build artifacts [$(Get-Date -Format 'yyyy-MM-dd HH:mm')]"
git push qdata main

Write-Host "✅ 全部完成！" -ForegroundColor Green