xdg-user-dirs:
	mkdir -p ~/doc ~/www ~/mus ~/img ~/vid ~/desk ~/pub ~/temp
	xdg-user-dirs-update --set DOCUMENTS ~/doc
	xdg-user-dirs-update --set DOWNLOAD ~/www
	xdg-user-dirs-update --set MUSIC ~/mus
	xdg-user-dirs-update --set PICTURES ~/img
	xdg-user-dirs-update --set VIDEOS ~/vid
	xdg-user-dirs-update --set DESKTOP ~/desk
	xdg-user-dirs-update --set PUBLICSHARE ~/pub
	xdg-user-dirs-update --set TEMPLATES ~/temp
