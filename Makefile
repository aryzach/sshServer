ssh:
	ssh -i ~/.ssh/awskey.pem -p 2022 ubuntu@13.52.217.172

run: 
	stack run main

build: 
	stack build

serve-public:
	sudo -u ubuntu authbind .stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/main/main

replace-service:
	sudo cp sshServer.service /etc/systemd/system/
	sudo systemctl daemon-reload
	sudo systemctl enable sshServer
	stack build
	sudo systemctl restart sshServer
