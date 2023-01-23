# `loadout.csv`

|variable      |class     |description   |
|:-------------|:---------|:-------------|
|version       |character |Country code for the version of the show     |
|season        |double    |The season number        |
|name          |character |Name of the survivalist          |
|item_number   |double    |Item number   |
|item_detailed |character |Detailed loadout item description |
|item          |character |Loadout item. Simplified for aggregation          |

# `episodes.csv`

|variable               |class     |description            |
|:----------------------|:---------|:----------------------|
|version                |character |Country code for the version of the show             |
|season                 |double    |The season number                 |
|episode_number_overall |double    |Episode number across seasons |
|episode                |double    |Episode                |
|title                  |character |Episode title                  |
|air_date               |double    |Date the episode originally aired               |
|viewers                |double    |Number of viewers in the US (millions)                |
|quote                  |character |The beginning quote                  |
|author                 |character |Author of the beginning quote                 |
|imdb_rating            |double    |IMDb rating of the episode            |
|n_ratings              |double    |Number of ratings given for the episode              |

# `seasons.csv`

|variable      |class     |description   |
|:-------------|:---------|:-------------|
|version       |character |Country code for the version of the show      |
|season        |double    |The season number        |
|location      |character |Location      |
|country       |character |Country       |
|n_survivors   |double    |Number of survivalists in the season. In season 4 there were 7 teams of 2.   |
|lat           |double    |Latitude           |
|lon           |double    |Longitude           |
|date_drop_off |double    |The date the survivalists were dropped off |
