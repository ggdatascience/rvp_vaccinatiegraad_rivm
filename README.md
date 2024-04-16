# rvp_vaccinatiegraad_rivm
Script voor GGD'en om RIVM gegevens met RVP vaccinatiegraden per PC4 en gemeente om te zetten naar een markdownrapportage met een leafletkaart.


In het setup R codeblock kan je een de instellingen aanpassen. Onder andere:
- Het minimum aantal observaties om een vaccinatiegraad te laten zien.
- Het minimum aantal observaties waaronder een waarschuwing wordt afgegeven.
- de selectie van cohorten, vaccinsoorten en vaccinatiestatussen voor de rapportage.

*Het script staat nu ingesteld op Pneumokokken voor Zuigelingen.*


> [!IMPORTANT]
> - De data voor je regio moet je zelf opvragen bij het RIVM.
> - Shapefiles moet je zelf toevoegen in /shapefiles. [Arcgis PC4 postcodevlakken](https://hub.arcgis.com/datasets/4bfb07de89954f5c8404b3fa2845c010) en [gpkg CBS gebiedsindelingen 2022](https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/cbs-gebiedsindelingen). Shapefiles worden ingelezen in lees_vaccinatiedata_rivm.R
> - Let er op dat je alle instellingen leest in het 1e/setup codeblock.
> - Pas de teksten en toelichtingen aan zodat je weet wat er staat en het klopt met wat je gemaakt hebt.
> - Kijk je output na.

![image](https://github.com/ggdatascience/rvp_vaccinatiegraad_rivm/assets/44730789/83cb3be8-3c22-45fd-92f7-6d1007880bfb)

![image](https://github.com/ggdatascience/rvp_vaccinatiegraad_rivm/assets/44730789/62554f06-f779-46a0-8c83-6844184fd6bb)
