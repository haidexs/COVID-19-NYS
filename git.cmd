git add .
date |awk 'BEGIN{FS=" "}{print "git commit -m \""$2,$3" updated by ZY\""}' | sh
git push
