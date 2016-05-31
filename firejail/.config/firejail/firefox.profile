# Firejail profile for Mozilla Firefox (Iceweasel in Debian)

noblacklist ~/.mozilla
noblacklist ~/.cache/mozilla
include /etc/firejail/disable-common.inc
include /etc/firejail/disable-programs.inc
include /etc/firejail/disable-devel.inc

caps.drop all
seccomp
protocol unix,inet,inet6,netlink
netfilter
tracelog
noroot

whitelist ${DOWNLOADS}
mkdir ~/.mozilla
whitelist ~/.mozilla
mkdir ~/.cache
mkdir ~/.cache/mozilla
mkdir ~/.cache/mozilla/firefox
whitelist ~/.cache/mozilla/firefox
whitelist ~/.vimperatorrc
whitelist ~/.vimperator
whitelist ~/.pentadactylrc
whitelist ~/.pentadactyl

include /etc/firejail/whitelist-common.inc

private ~/.firejail-profiles/firefox/main

blacklist /mnt
blacklist /media
blacklist /srv
blacklist /run/media
blacklist /opt

# experimental features
private-etc passwd,group,hostname,hosts,localtime,nsswitch.conf,resolv.conf,gtk-2.0,gtk-3.0,pango,fonts,iceweasel,firefox,adobe,mime.types,mailcap,asound.conf,pulse

private-tmp
private-dev
# private-bin
