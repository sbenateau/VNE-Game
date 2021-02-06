# VNE-Game
A serious game for the participants of Vigie-Nature École.

This game aims to develop data literacy for highschool students.

We developed a game using paper cards in order to change the type of human/machine interactions.

The aim of the game is to analyse data. First, check that you have sufficient data, that the spatial cover is large enough and then answer questions like : "Does the type of environment influence the bird populations ?"


## A link to the application now running on the VNE shiny server.
https://shiny.vigienature-ecole.fr/sample-apps/VNE-Game/


# Install

## get the repo

You have different option to install this app

1.  Clone this repo

In an Rstudio terminal or a normal terminal:

``` bash
git clone https://github.com/sbenateau/duduch.git
```

2.  Just download it

## Install the python part (only for the card detection part)

``` bash
python3 -m venv opencv
. opencv/bin/activate
pip install opencv-contrib-python
```

# Launch

```r
shiny::runApp("~/my/path/to/the/repo/VNEGame/ui.R")
```
