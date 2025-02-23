#!/bin/bash

playerctl metadata --format '󰎈 {{title}} - {{artist}}' | sed 's/&/and/g'
