(ns culture.facts
  "Country-level regional-culture catalog for Morocco (MAR) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"MAR"
   [{:culture/id "mar.dish.tajine"
     :culture/name "Tagine"
     :culture/name-local "Tajine"
     :culture/country "MAR"
     :culture/kind :dish
     :culture/summary "Slow-cooked Maghrebi stew prepared in a distinctive cone-shaped earthenware pot of the same name."
     :culture/url "https://en.wikipedia.org/wiki/Tajine"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "mar.dish.couscous"
     :culture/name "Couscous"
     :culture/country "MAR"
     :culture/kind :dish
     :culture/summary "Traditional North African dish of small steamed granules of rolled semolina, often served with a stew spooned on top; a staple throughout Maghrebi cuisines including Morocco."
     :culture/url "https://en.wikipedia.org/wiki/Couscous"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "mar.dish.pastilla"
     :culture/name "Pastilla"
     :culture/country "MAR"
     :culture/kind :dish
     :culture/summary "North African meat or seafood pie of Maghrebi cuisine made with warqa dough, similar to filo."
     :culture/url "https://en.wikipedia.org/wiki/Pastilla"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "mar.beverage.mint-tea"
     :culture/name "Moroccan mint tea"
     :culture/country "MAR"
     :culture/kind :beverage
     :culture/summary "North African preparation of gunpowder green tea with spearmint leaves and sugar, central to social life in the region."
     :culture/url "https://en.wikipedia.org/wiki/Maghrebi_mint_tea"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "mar.product.argan-oil"
     :culture/name "Argan oil"
     :culture/country "MAR"
     :culture/kind :product
     :culture/summary "Plant oil produced from the kernels of the argan tree (Argania spinosa), indigenous to Morocco, used for both culinary and cosmetic purposes."
     :culture/url "https://en.wikipedia.org/wiki/Argan_oil"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "mar.craft.zellij"
     :culture/name "Zellij"
     :culture/country "MAR"
     :culture/kind :craft
     :culture/summary "Mosaic tilework made from individually hand-chiseled tile pieces featuring geometric Islamic patterns; originated in North Africa and remains a living craft tradition primarily in Morocco today."
     :culture/url "https://en.wikipedia.org/wiki/Zellij"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "mar.festival.imilchil-marriage-festival"
     :culture/name "Imilchil Marriage Festival"
     :culture/name-local "Souk Aam / Agdoud N'Oulmghenni"
     :culture/country "MAR"
     :culture/kind :festival
     :culture/summary "Annual betrothal festival in Imilchil, a symbol of Berber culture, where up to 40 couples take their vows on the same day amid music, dancing and feasts."
     :culture/url "https://en.wikipedia.org/wiki/Imilchil"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "mar.heritage.fes-el-bali"
     :culture/name "Fes el Bali"
     :culture/country "MAR"
     :culture/kind :heritage
     :culture/summary "Oldest part of Fez, Morocco, inscribed as a UNESCO World Heritage Site in 1981 as part of the Medina of Fez."
     :culture/url "https://en.wikipedia.org/wiki/Fes_el_Bali"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-mar culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "MAR"))
                 " MAR entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))
