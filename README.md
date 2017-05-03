# Tree volume, biomass and carbon

Here you can find a collection of allometric equations to calculate volume of trees. Equations have been gathered from all over Europe and are applied to the data according to the following graph.

![Decission tree to chose relevant allometic equations for calculation of tree volume](presentations/decission-tree%20equations_vertical.png?raw=true)

A 'global equation' is any equation for the calculation of stemwood volume of a specific species that has been defined somewhere in the literature. 'Generalising' a species refers to considering only the genus of this species and 'generalisation' is possible, if several tree species within a genus exist and the genus only is described by an equation (such as in the case of *Quercus* or *Acer*). 'Substituting' a species is possible, when it is reasonable to assume that a very similar species exists (such as in the case of *Quercus* sp. and *Castanea sativa*).

Equations have originally been pulled from Zianis et al. (2005) and the [Globallometree](http://www.globallometree.org/)-database. However, equations from globallometree.org had to be corrected on several occasions.

Equations were harmonized to be giving the same units for volume (m³). Biomass is calculated utilizing BCEF-values from Aalde et al. (2006) (Tab. 4.5) and root-biomass is calculated utilizing root-shoot values from Mokany et al. (2006). Carbon was calculated by multiplying total biomass with 0.5.

## Usage
Everything can be used, under the terms of the [License](LICENSE). You only need to pay attention to either set ID of species in your data to the values that can be found [here](tables/tree_species.csv) or adapt the code.
Feel free to fork it all and add your equations and values to this repo!

Read in data via a data.frame of the following form:

| id_location | id_species | d130 | height | other |
| ----- | ----- | ----- | ----- | ----- |
| location 1 | species 1 | 41 | 22.5 | ... |
| location 1 | species 2 | 13 | 8.3 | ... |
| ... | ... | ... | ... | ... |

_id_location_ and _other_ can be any desired number of additional columns for the needs of your sampling design. _id_species_, _d130_ and _height_ however are required to run this function.

## References
- Børset, O. (1954): Kubering av osp på rot. Meddelelser fra det norske Skogforsøksvesen 12: 391–447.
- Braastad, H. (1966): Volumtabeller for bjørk. Meddelelser fra det Norske Skogforsøksvesen 21(1): 23–78.
- Dagnelie, P., Palm, R., Rondeux, J. & Thill, A. (1999): Tables de cubage des arbres et des peuple- ments forestiers. Les Presses Agronomiques de Gembloux, Gembloux. 126 p.
- Dik, E.J. (1984): Estimating the wood volume of standing trees in forestry practice. Rijksinstituut voor onderzoek in de bos en landschapsbouw de Dorschkamp, Wageningen. Uitvoerige verslagen 19(1): 1–114.
- Eriksson, H. (1973): Volymfunktioner för ståendeträd av ask, asp, klibbal och contorta-tall. Institutionen för Skogsproduktion, Royal College of Forestry, Stockholm. Research Notes 26: 1–26.
- Estonian forest inventory (ask for additional data from Jaan Liira)
- Giurgiu, V. (1974): O expresie matematica unica a relatiei diametru – înaltime – volum, pentru majori- tatea speciilor forestiere din Romania. Silvicultura si Exploatarea Padurilor 89(4): 173–178.
- Näslund, M. (1947): Funktioner och tabeller för kubering av stående träd. Meddelanden från Statens skogsforskningsinstitutet 36(3): 1–81.
- Øen, S., Bauger, E. & Øyen, B.-H. (2001): Functionar for volumberekning av framande treslag i Vest-Norge. Aktuelt fra Skogforsk 3/01: 18–19.
- Pellinen, P. (1986): Biomasseuntersuchungen im Kalkbuchenwald. University of Göttingen, Germany. 145 p.
- Schelhaas, M.J., Nabuurs, G.J., Jans, W.W.P., Moors, E.J., Sabaté, S. & Daamen, W.P. (2002): Converging estimates of the forest carbon sink. Alterra-rapport 631: 1–44.
- Zianis, D., Muukkonen, P., Mäkipää, R., Mencuccini, M. (2005): Biomass and stem volume equations for tree species in Europe. Silva Fennica Monographs 4

### BCEFs (and carbon fraction)
- Aalde et al. (2006) - IPCC Guidelines for National Greenhouse Gas Inventories (Tables 4.4 and 4.3)

### root:shoot ratio
- Mokany, Karel, Raison, R. John, Prokushkin, Anatoly S. (2006): Critical analysis of root shoot ratios in terrestrial biomes. Global Change Biology 12: 84–96.
