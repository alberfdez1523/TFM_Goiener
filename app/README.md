# GoiEner TFM Dashboard

Dashboard estatico en Astro para consultar los resultados agregados del TFM.

## Desarrollo local

```powershell
Set-Location app
npm install
npm run sync:data
npm run dev
```

## Build

```powershell
Set-Location app
npm run build
npm run preview
```

Los CSV que consume la app viven en `app/public/data/`. Si regeneras los
resultados del pipeline, ejecuta `npm run sync:data` desde esta carpeta para
actualizar la copia publica.
