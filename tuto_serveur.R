# se connecter au serveur
ssh root@server.vigienature-ecole.fr

# aller voir les logs
cd /var/log/shiny-server

# les scripts
cd /var/www/others/formations_vne/shiny/

# éditer la configuration du serveur
nano /etc/shiny-server/shiny-server.conf

# pour ajouter par exemple :
preserve_logs true; # permet de concerver les log en cas de plantage

# redémarrer le serveur 
service shiny-server restart
# démarrer le serveur
service shiny-server start

# sortir du serveur
exit