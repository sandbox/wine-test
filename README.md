# Wine Tasting

Scripts and visualizations to analyze a wine tasting experiment.

# Sample Data

* samples/survey_1_2.csv
  * station_id,experiment_no
    * 1A,1 => votes_1 == A, votes_2 == B
    * 1A,2 => votes_1 == A, votes_2 == C
    * 1B,1 => votes_1 == A, votes_2 == B, added_1 == votes added to votes_1 == A
    * 1B,2 => votes_1 == C, votes_2 == A, added_1 == votes added to votes_1 == C
    * 2A,1 => votes_1 == A, votes_2 == C special
    * 2B,2 => votes_1 == A, votes_2 == B special
  * votes_1
  * votes_2
  * added_1

* samples/survey_3.csv
  * treament - indicates first or second half of the experiment
  * wine - the wine shown
    * 1 - earthy picture (D when treatment==1, E when treatment==2),
    * 2 - berry picture  (D when treatment==2, E when treatment==1)

    wine,treatment
    1,1   =>  D earthy
    1,2   =>  E earthy
    2,1   =>  E berry
    2,2   =>  D berry
    
  * 1,...,12 - the row index of the word used (lookup in `samples/descriptors.csv`)

* samples/descriptors.csv
  * word - the word being used to describe.  The row index can be used as a key to find out what word we are referring to in `survey_3.csv`.
  * weight - the associated sweetness weight of the word.  This is subjective but it should be a rough measure of how much the word connotates sweetness.
