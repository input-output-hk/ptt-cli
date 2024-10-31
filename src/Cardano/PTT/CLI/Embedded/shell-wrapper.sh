#!/bin/bash

# Set the path to your Nix project
# NIX_PROJECT_PATH="/Users/horus/iog/minimal-ptt-examples-new"
NIX_PROJECT_PATH="$projectPath"
cd $$NIX_PROJECT_PATH

# Create a temporary script file
TEMP_SCRIPT=$(mktemp)

# Write commands to the temporary script
cat << EOF > $$TEMP_SCRIPT
#!/bin/sh

# Check if cabal is available
if ! command -v cabal &> /dev/null; then
    echo "Error: cabal is not available in this Nix shell"
    exit 1
fi

cd $sourcePath
# Print current PATH for debugging
echo "Current PATH: $$PATH"
echo "Current folder: $(pwd)"

# Execute cabal commands
echo "Running cabal build..."
echo "cabal build $testSuite"
cabal build $testSuite

echo "Running cabal test..."
echo "$testExeLine"
echo ">>>>>START"
$testExeLine
echo "<<<<<<END"
# Exit the Nix shell
exit
EOF

# Make the temporary script executable
chmod +x $$TEMP_SCRIPT

OS_TYPE=$(uname)

if [[ "$$OS_TYPE" == "Darwin" ]]; then
    echo "Running on macOS"
    NIX_CONFIG="system = x86_64-darwin"
elif [[ "$$OS_TYPE" == "Linux" ]]; then
     echo "Running on Linux"
else
    echo "Unsupported OS: $$OS_TYPE" 1>&2
    exit 1
fi

# Execute the Nix command with the temporary script
NIX_CONFIG=$$NIX_CONFIG nix develop . --command sh $$TEMP_SCRIPT

# Clean up the temporary script
rm $$TEMP_SCRIPT
