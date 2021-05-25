# Bootstrap-model-selection
In a regression problem, typically there are p explanatory variables possibly related to a response variable, and we wish to select  a subset of the p explanatory variables to fit a model between these variables and the response. A bootstrap variable/model  selection procedure is to select the subset of variables by minimizing bootstrap estimates of the prediction error, where the  bootstrap estimates are constructed based on a data set of size n. Although the bootstrap estimates have good properties, this  bootstrap selection procedure is inconsistent in the sense that the probability of selecting the optimal subset of variables does  not converge to 1 as n → ∞. This inconsistency can be rectified by modifying the sampling method used in drawing bootstrap observations. For bootstrapping pairs (response, explanatory variable), it is found that instead of drawing n bootstrap observations  (a customary bootstrap sampling plan), much less bootstrap observations should be sampled.


This project is a part of journal article presentation done by Narambatla Samanvitha, Runa Veigas, and Vaishak N of Department of Data Science, PSPH, Manipal Academy of Higher education under the guidance of Mr. Himanshu Pokhriyal, Assistant Professor, Department of Data Science, PSPH, Manipal Academy of Higher education. The orginal article is available at References Section.

## References
<a id="1">[1]</a> 
Shao, J. (1996). Bootstrap Model Selection. Journal of the American Statistical Association, 91(434), 655-665.
doi:10.2307/2291661
