#!/bin/bash
echo $HOME
EXE=taketa_make_db_race
OPTIONS='--config /home/eric/1_Projects/auto/par/0_com/config.nml --mode routine'

chmod -R 777 "${HOME}/1_Projects"
chmod -R 777 "${HOME}/3_Data"
chmod -R 777 "/var/www/auto"

cat <<EOF>/etc/systemd/system/${EXE}.service
[Unit]
Description=${EXE}

[Service]
ExecStart=/usr/bin/${EXE} ${OPTIONS}
Restart=always

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl stop ${EXE}.service
make install
systemctl enable ${EXE}.service
systemctl start ${EXE}.service
systemctl status ${EXE}.service

ps aux | rg ${EXE} 
