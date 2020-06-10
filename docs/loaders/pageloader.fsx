#r "../_lib/Fornax.Core.dll"


type Shortcut = {
    title: string
    link: string
    icon: string
}

let loader (projectRoot: string) (siteContet: SiteContents) =
    siteContet.Add({title = "Home"; link = "/"; icon = "fas fa-home"})
    siteContet.Add({title = "GitHub repo"; link = "https://github.com/MoiraeSoftware/myriad"; icon = "fab fa-github"})
    siteContet