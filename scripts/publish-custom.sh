#!/bin/bash
# Publish script for custom ReScript package

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}Publishing custom ReScript packages...${NC}"

# Check if logged into npm
if ! npm whoami > /dev/null 2>&1; then
    echo -e "${RED}Error: Not logged into npm. Run 'npm login' first.${NC}"
    exit 1
fi

NPM_USER=$(npm whoami)
echo -e "${GREEN}Logged in as: $NPM_USER${NC}"

# Get version from root package.json
VERSION=$(node -p "require('./package.json').version")
echo -e "${YELLOW}Publishing version: $VERSION${NC}"

# Step 1: Publish darwin-arm64 package
echo -e "\n${YELLOW}Step 1: Publishing @roshan84ya/rescript-darwin-arm64...${NC}"
cd packages/@rescript/darwin-arm64

# Update version
node -e "const fs = require('fs'); const pkg = JSON.parse(fs.readFileSync('package.json', 'utf8')); pkg.version = '$VERSION'; fs.writeFileSync('package.json', JSON.stringify(pkg, null, 2) + '\n');"

# Publish
npm publish --access public --tag custom
echo -e "${GREEN}✓ Published @roshan84ya/rescript-darwin-arm64@$VERSION${NC}"

cd ../../..

# Step 2: Publish main package
echo -e "\n${YELLOW}Step 2: Publishing @roshan84ya/rescript...${NC}"

# Backup original package.json
cp package.json package.json.backup

# Use clean publish package.json
cp scripts/publish-package.json package.json

# Update version
node -e "const fs = require('fs'); const pkg = JSON.parse(fs.readFileSync('package.json', 'utf8')); pkg.version = '$VERSION'; pkg.optionalDependencies['@roshan84ya/rescript-darwin-arm64'] = '$VERSION'; fs.writeFileSync('package.json', JSON.stringify(pkg, null, 2) + '\n');"

# Publish
npm publish --access public --tag custom
echo -e "${GREEN}✓ Published @roshan84ya/rescript@$VERSION${NC}"

# Restore original package.json
mv package.json.backup package.json

echo -e "\n${GREEN}=========================================${NC}"
echo -e "${GREEN}All packages published successfully!${NC}"
echo -e "${GREEN}=========================================${NC}"
echo -e "\n${YELLOW}Published packages:${NC}"
echo -e "  - @roshan84ya/rescript-darwin-arm64@$VERSION"
echo -e "  - @roshan84ya/rescript@$VERSION"
echo -e "\n${YELLOW}Other platform packages will be installed from official registry:${NC}"
echo -e "  - @rescript/darwin-x64@12.1.0"
echo -e "  - @rescript/linux-arm64@12.1.0"
echo -e "  - @rescript/linux-x64@12.1.0"
echo -e "  - @rescript/win32-x64@12.1.0"
echo -e "\n${YELLOW}To install your custom package:${NC}"
echo -e "  npm install @roshan84ya/rescript@$VERSION"
