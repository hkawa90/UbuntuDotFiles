# Install
sudo apt-get update

## Fonts
mkdir $HOME/.fonts/
cd $HOME/.fonts/
wget -q https://github.com/miiton/Cica/releases/download/v5.0.2/Cica_v5.0.2_with_emoji.zip
unzip -q ../Cica_v5.0.2_with_emoji.zip -d Cica
cd $HOME

## Gcc, make
sudo apt install build-essential git
## Emacs
sudo apt install zile
sudo apt install emacs-mozc-bin
sudo snap install emacs --classic --beta
sudo apt install pandoc
sudo apt install ripgrep fd-find
## Utils
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
