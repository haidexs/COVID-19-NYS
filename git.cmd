git add .
date |gawk 'BEGIN{FS=" "}{print "git commit -m \""$2,$3" updated by ZY\""}' | sh
git push
