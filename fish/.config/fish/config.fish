# source profile
if status --is-login && test -z $fish
	set -x fish flounder
	exec sh -l -c 'exec fish -l'
end
