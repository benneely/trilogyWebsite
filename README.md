# trilogyWebsite
R Shiny app interface to the TRILOGY ACS prediction models in publication. [Demo here](http://trilogyacs-bleedingmodel-01.oit.duke.edu/).

## Docker
Here are the commands to stand up the web app(s).

```
cd ./trilogyWebsite
docker build -t trilogywebsite .

docker run -d -p 80:3838 \
-v $(pwd):/srv/shiny-server/ \
--restart always \
trilogywebsite

```
