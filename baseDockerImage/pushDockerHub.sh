echo "push to dockerhub"
docker build --build-arg CACHEBUST=$(date +%s) .
docker images
echo "docker tag <tag> sujaytalesara/Cyclometric-Complexity"
echo "docker push sujaytalesara/Cyclometric-Complexity"
