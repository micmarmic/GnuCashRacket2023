 #lang racket

#|
1. ASSET ALLOCATION

TARGET ALLOCATION  CA   US   INTL  OTHER
                  50%  50%      0      0

2.ALLOCATION TABLE CA   US   INTL  OTHER
Asset 1            100%
Asset 2                 100%
Asset 3             20%  20%   40%   20%

3. REPORT
                            ALLOCATED AMOUNTS
ACCOUNT 1      Amount    CA   US   INTL  OTHER
Asset 1        1000    1000
Asset 2        1000          1000
Totals         2000    1000  1000


ACCOUNT 2      Amount    CA   US   INTL  OTHER
Asset 1        1000    1000
Asset 3        1000     200   200   400    200 
Totals         2000    1200   200   400    200

--- SUMMARY ---
GRAND TOTALS   4000    2200  1200   400    200
ALLOCATION %   100%     55%   30%   10%     5%
TARGET     %   100%     50%   50%
DIFFERENCE %            -5%  +20%  -10%    -5%  
DIFFERENCE $           -$200 +$800 -$400  -$200

4. INPUT SCENARIOS

ASSET   AMOUNT
Asset1    -100

--- REVISED SUMMARY ---
MET


#|