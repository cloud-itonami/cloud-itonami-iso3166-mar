(ns marketentry.facts "Morocco market-entry catalog.")
(def catalog
  {"MAR" {:name "Morocco"
          :owner-authority "Trésorerie Générale / portail des marchés publics"
          :legal-basis "Décret relatif aux marchés publics"
          :national-spec "portail des marchés publics + ICE/RC"
          :provenance "https://www.marchespublics.gov.ma/"
          :required-evidence ["ICE/RC record" "e-procurement registration record" "Commercial registry extract" "Authorized-representative record"]
          :rep-owner-authority "contracting authorities / Trésorerie"
          :rep-legal-basis "Moroccan legal entity registration typically required for public awards"
          :rep-provenance "https://www.marchespublics.gov.ma/"
          :corporate-number-owner-authority "OMPIC / DGI"
          :corporate-number-legal-basis "ICE / RC number"
          :corporate-number-provenance "https://www.ompic.ma/"}
   "USA" {:name "United States" :owner-authority "GSA/SAM.gov" :legal-basis "FAR" :national-spec "SAM.gov" :provenance "https://sam.gov/"
          :required-evidence ["EIN record" "SAM.gov registration record" "State business registration record" "SAM UEI verification record"]}
   "FRA" {:name "France" :owner-authority "PLACE" :legal-basis "Code de la commande publique" :national-spec "PLACE" :provenance "https://www.marches-publics.gouv.fr/"
          :required-evidence ["SIRET record" "PLACE registration" "RCS extract" "Authorized-representative record"]}
   "EGY" {:name "Egypt" :owner-authority "e-Tenders" :legal-basis "Law 182/2018" :national-spec "e-Tenders" :provenance "https://etenders.gov.eg/"
          :required-evidence ["Commercial registry" "e-Tenders registration" "Tax card" "Authorized-representative record"]}})

(defn spec-basis [iso3] (get catalog iso3))
(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s) missing (remove catalog iso3s)]
     {:requested (count iso3s) :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note "R0 catalog seed"})))
(defn required-evidence-satisfied? [iso3 submitted]
  (when-let [{:keys [required-evidence]} (spec-basis iso3)]
    (= (count required-evidence) (count (filter (set submitted) required-evidence)))))
(defn evidence-checklist [iso3] (:required-evidence (spec-basis iso3) []))
(defn rep-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:rep-owner-authority sb)
      (select-keys sb [:rep-owner-authority :rep-legal-basis :rep-provenance]))))
(defn corporate-number-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:corporate-number-owner-authority sb)
      (select-keys sb [:corporate-number-owner-authority :corporate-number-legal-basis :corporate-number-provenance]))))
