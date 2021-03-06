# Install
sudo apt-get update

## Fonts
mkdir $HOME/.fonts/
cd $HOME/.fonts/
mkdir Cica
cd Cica
wget -q https://github.com/miiton/Cica/releases/download/v5.0.2/Cica_v5.0.2_with_emoji.zip
unzip -q Cica_v5.0.2_with_emoji.zip -d Cica
cd ..
mkdir SourceCode
cd SourceCode
wget https://github.com/adobe-fonts/source-code-pro/releases/download/2.032R-ro%2F1.052R-it%2F1.012R-VAR/TTF-source-code-pro-2.032R-ro-1.052R-it.zip
unzip -q TTF-source-code-pro-2.032R-ro-1.052R-it.zip
cd ../
mkdir Fira
cd Fira
wget -q https://github.com/tonsky/FiraCode/releases/download/5.2/Fira_Code_v5.2.zip
-unzip -q Fira_Code_v5.2.zip
fc-cache -f
cd $HOME

## Gcc, make
sudo apt install build-essential git
## Emacs
sudo apt -y install zile
sudo apt -y install emacs-mozc-bin
sudo snap install emacs --classic --beta
sudo apt -y install pandoc
sudo apt -y install ripgrep fd-find
## Utils
### Graphviz
sudo apt -y install graphviz
### Gnuplot
sudo apt -y install gnuplot-qt
### Google Drive
apt-get install google-drive-ocamlfuse
### 付箋紙
sudo add-apt-repository ppa:umang/indicator-stickynotes
sudo apt update
sudo apt -y install indicator-stickynotes
## Node.js
### Nvm
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
nvm install stable --latest-npm
nvm alias default stable
### pnpm
npm install -g pnpm
## typescript-language-server
npm install -g typescript-language-server
# Docer
## See https://docs.docker.com/engine/install/debian/
sudo apt-get -y install \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common
curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -
sudo add-apt-repository \
   "deb [arch=arm64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
sudo apt-get update
sudo apt-cache policy docker-ce
sudo apt-get install docker-ce docker-ce-cli containerd.io
sudo usermod -aG docker ${USER}
# build system
## Scons
sudo apt-get -y install scons
## ccls (language server)
sudo apt -y install ccls
## clang
sudo apt -y install clang
# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
## Rustfmt
cargo install --force cargo-make
wget https://github.com/rust-lang/rustfmt/archive/v1.4.31.zip
unzip v1.4.31.zip
cd rustfmt-1.4.31
cargo make install
cd ..
rustup component add rls
wget https://github.com/rust-analyzer/rust-analyzer/archive/2021-01-11.zip
unzip 2021-01-11.zip
cd rust-analyzer-2021-01-11
cargo xtask install

## My Project
sudo apt-get -y install libconfuse-dev
sudo apt-get -y install libiberty-dev
sudo apt-get -y install uuid-dev
sudo apt-get -y install uuid-dev meson 
sudo apt-get -y install clang
sudo apt-get -y ninja-build
sudo apt-get -y ccache
sudo apt-get -y cmake
