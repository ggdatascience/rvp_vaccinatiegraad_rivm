# rvp_vaccinatiegraad_rivm
Script voor GGD'en om RIVM gegevens met RVP vaccinatiegraden per PC4 en gemeente om te zetten naar een markdownrapportage met een leafletkaart.


In het setup R codeblock kan je een de instellingen aanpassen. Onder andere:
- Het minimum aantal observaties om een vaccinatiegraad te laten zien.
- Het minimum aantal observaties waaronder een waarschuwing wordt afgegeven.
- de selectie van cohorten, vaccinsoorten en vaccinatiestatussen voor de rapportage.

Het script staat nu ingesteld op Pneumokokken voor Zuigelingen.


> [!IMPORTANT]
> - De data voor je regio moet je zelf opvragen bij het RIVM.
> - Shapefiles voor PC4 zitten in de repo. De cbs-gebiedsindelingen kan je [downloaden bij het cbs](https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/cbs-gebiedsindelingen) 
> - Let er op dat je alle instellingen leest in het setup codeblock.
> - Pas de teksten en toelichtingen in het .qmd aan.
> - Kijk je output na.

![image](https://github.com/ggdatascience/rvp_vaccinatiegraad_rivm/assets/44730789/83cb3be8-3c22-45fd-92f7-6d1007880bfb)

