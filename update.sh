set -e
cd PA_evictions2
git pull --ff-only
Rscript script.R
git commit -am "update data"
git push
