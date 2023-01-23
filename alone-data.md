### Data Dictionary


# `survivalists.csv`

|variable            |class     |description         |
|:-------------------|:---------|:-------------------|
|season              |double    |The season number              |
|name                |character |Name of the survivalist                |
|age                 |double    |Age of the survivalist                 |
|gender              |character |Gender              |
|city                |character |City                |
|state               |character |State               |
|country             |character |Country             |
|result              |double    |Place survivalist finished in the season              |
|days_lasted         |double    |The number of days lasted in the game before tapping out or winning         |
|medically_evacuated |logical   |If the survivalist was medically evacuated from the game |
|reason_tapped_out   |character |The reason the survivalist tapped out of the game. NA means they were the winner. Reason being that technically if they won they never tapped out.   |
|reason_category     |character |A simplified category of the reason for tapping out  |
|team                |character |The team they were associated with (only for season 4)            |
|day_linked_up       |double    |Day the team members linked up (only for season 4)       |
|profession          |character |Profession          |
|url                 |character |URL of cast page on the history channel website. Prefix URL with https://www.history.com/shows/alone/cast                 |
