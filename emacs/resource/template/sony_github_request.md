#### [JIRA]

* https://www.tool.sony.biz/jira3/browse/`(replace-regexp-in-string ".+/" "" (fc-git-current-branch))`

#### [概要/Summary]

* $1

#### [火入れ確認/Confirmation Method]

* $2

#### [修正内容の妥当性/Validation of the modification]

* $3

#### [修正による弊害が無いことの確認/Degrade Check]

Basic Alexa operations have been confirmed
 * [X] wake-word-to-speech twice in a row
 * [X] tap-to-speech then wake-word-to speech, twice in a row
 * [X] turn off then turn on, ama is reconnected
 * [X] turn off bluetooth while speech, then turn on, make sure ama works
 * [X] call-in while ama speech, confirm the speech is ended and phone call is OK

#### [検討したエラーケースを記載/Error case Check]
