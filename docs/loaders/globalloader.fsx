#r "../_lib/Fornax.Core.dll"

type SiteInfo = {
    title: string
    description: string
    theme_variant: string option
    root_url: string
}

let config = {
    title = "Myriad"
    description = "Myriad is code generator for F#"
    theme_variant = Some "red"
    root_url =
      #if WATCH
        "http://localhost:8080/"
      #else
        "https://moiraesoftware.github.io/myriad/"
      #endif
}

let loader (projectRoot: string) (siteContet: SiteContents) =
    siteContet.Add(config)

    siteContet
