# Uncertain emotion discrimination differences between musicians and nonmusicians is determined by fine structure association: Hilbert transform psychophysics  

**Authors:** Francis A.M. Mannoa,b,†, Raul R. Crucesc, Condon Laub,†, Fernando A. Barriosc,†  
**Contact:** Francis.Manno@nyu.edu, fbarrios@unam.mx, condon.lau@cityu.edu.hk  
  
**OSF:** https://osf.io/8ws7a  
**Keywords:** emotion; psychophysics; modulation; fine structure; envelope; frequency; amplitude  
  
# Abstract
We perceive musical sound as a complex phenomenon, which is known to induce an emotional response in humans. The cues used to perceive emotion in music have not been unequivocally elucidated. Here, we sought to identify the attributes of sound that confer an emotion to music and determine if professional musicians have different musical emotion perception than nonmusicians. The objective was to determine which sound cues are used to resolve emotional signals. Happy or sad classical music excerpts modified in fine structure or envelope conveyed different degrees of emotional certainty. The psychophysical emotional response of the modified excerpts was measure based on the originals. Certainty was determined by identification of the emotional characteristic presented during a forced-choice discrimination task. Participants were categorized as good or poor performers (n = 32, age 21.17 ± 2.63 SD) and in a separate group as musicians in the first or last year of music education at a conservatory (n = 32, age 21.97 ± 2.42). We found that temporal fine structure information is essential for correct emotional identification. Non-musically educated individuals used less fine structure information to discriminate emotion in music compared with musically educated individuals. The present psychophysical experiments revealed what cues are used to resolve emotional signals and how they differ between nonmusicians and musically educated individuals.  

# Code and Methods  
  
## Canonical Discriminant Analysis  
Canonical Discriminant Analysis
Two generalized canonical discriminant analysis was compute using the multivariate linear model Group~nb0+nb2+nb4+nb8+nb16+nb32+nb64 to obtain the canonical scores and vectors, one for HAPPY and the other for SAD. It represents a transformation of the original variables in the scpace of maximal differences for the group. 
THe biplot shows the canonical scores for the groups defined by the term as points and the canonical structure coefficients as vectors from th eorigin.  
  
 Standardized beta coefficients are given for each variable in each discriminant (canonical) function, and the larger the standardized coefficient, the greater is the contribution of the respective variable to the discrimination between groups. However, these coefficients do not tell us between which of the groups the respective functions discriminate.  

**Standardized Coefficients: SAD**  

Stimuli | Can1 | Can2 | Can3  |
|---------- |:----------:|:----------:| ---------:|
|**nb0** | 0.6264 | 0.7113 | -0.3907 |
|**nb2** | -0.1806 | 0.3608 | 0.1146 |
|**nb4** | 0.507 | -0.6772 | -0.4067 |
|**nb8** | -0.5025 | 0.3103 | -0.6578 |
|**nb16** | -0.2266 | -0.4923 | -0.2479 |
|**nb32** | -0.05239 | 0.7701 | 1.224 |
|**nb64** | -0.7641 | -0.08169 | -0.24 |

  
  
**Standardized Coefficients HAPPY**

|Stimuli | Can1 | Can2 | Can3  |
|---------- |:----------:|:----------:| ---------:|
| **nb0** | -1.011 | -0.05111 | 0.04974 |
|**nb2** | -0.01097 | -0.1844 | 0.3217 | 
|**nb4** | 0.2952 | -0.4591 | -0.4602 |
|**nb8** | -0.08152 | 0.8909 | 1.472 |
|**nb16** | -0.2719 | 0.5762 | -1.304 |
|**nb32** | 0.06616 | -0.4042 | -0.1513 |
|**nb64** | 0.4193 | 0.4013 | -0.2076 |

  
The discriminant function coefficients denote the unique contribution of each variable to the discriminant function, while the structure coefficients denote the simple correlations between the variables and the functions


