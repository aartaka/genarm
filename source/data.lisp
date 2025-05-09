;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :genarm)

;; Better?
;; https://scholarworks.aub.edu.lb/bitstream/handle/10938/1313/t-977.pdf?sequence=1&isAllowed=y

;; TODO: :?
(defparameter
    *structures*
  ;; Noun phrase (:a is adjective)
  '(:np (:or :p (:pl :n) ((:* :a) :n) ((:* :a) (:pl :n)))
    ;; Subject phrase (:ar-ticle + noun)
    :sp (:or :p (:ar :n) ((:* :a) :n) ((:* :a) (:pl :n)))
    ;; Object phrase
    :op (:np)
    ;; Present/imperfect modal (:nt is negation)
    :modal (:or :m (:nt :m))
    :vv (:or :v (:pass :v))
    :present (:or
              ;; TODO: Adverbs and verb phrases
              ((:? :av) :sp (:pt :vv) :modal)
              ;; :pt present tense
              ((:? :av) :sp :op :modal (:pt :vv)))
    :future-simple (:or
                    ;; :sft simple future
                    (:sp (:sft :vv) :modal)
                    (:sp :op :modal (:sft :vv)))
    :future-common (:or
                    ;; :ft classic/common future
                    (:sp (:ft :vv))
                    (:sp :op (:ft :vv)))
    :past-simple (:or
                  ;; :pst simple past
                  (:sp (:pst :vv))
                  (:sp :op (:pst :vv)))
    :past-modal (:pst (:or (:nt :m) :m))
    :past-imperfect (:or
                     (:sp :vv :past-modal)
                     (:sp :op :past-modal :vv))
    :past-perfect (:or
                   (:sp :vv :past-modal)
                   (:sp :op :past-modal :vv))
    :present-perfect (:or
                      (:sp :vv :modal)
                      (:sp :op :modal :vv))
    :future-conditional  (:or
                          ;; :ct conditional tense
                          (:sp (:ct :vv))
                          (:sp :op (:ct :vv)))
    :sentence (:or
               :present :present-perfect
               :future-simple :future-common :future-conditional
               :past-simple :past-imperfect :past-perfect)))

(defparameter *adjectives*
  (remove-duplicates
   '("բութ" "մութ" "թուխ"
     "փակ" "փափուկ" "տաք" "թանկ"
     "լուռ" "սառ" "վառ" "հին" "հակառակ" "համով" "կաղ" "խաղաղ" "պաղ"
     "սպիտակ" "փոքր" "կարմիր" "ձախ" "ծուռ" "ջինջ" "լուրջ" "ճիշտ" "ճկուն" "ճշմարիտ" "կարճ" "երկար"
     "բոլոր" "հզոր" "նոր"
     "աշխատասեր" "լավ" "վատ" "հիվանդ" "տխուր" "նոր" "աջ" "ձախ" "ջինջ" "մոտ" "հեռու"
     "բարի" "ազնիվ" "սև" "երևանյան" "ամայի" "սրտային" "թվական")
   :test #'equal))

(defparameter *nouns*
  (remove-duplicates
   '("թաթ" "թութ" "թագ" "գութ" "գութան" "գագաթ" "գաթա" "կաթ" "թան" "թիմ" "թումբ"
     "փութ" "թափ" "թուփ" "տուփ" "ափ" "փափախ" "խուփ"
     "քիթ" "քուն" "բուք" "քամի" "քանի"  "քաթան" "քանակ" "քիմք" "քանդակ" "քունք" "քիմիա" "ունք"
     "դանակ" "տուն" "մատիտ" "աման" "բակ"
     "լուր" "սար" "տառ" "բառ" "բար" "վար" "առու" "արու"
     "հաբ" "հրապարակ" "հազ" "համալսարան" "հազար" "սահման" "նախագահ" "մահ" "սահնակ" "համ"
     "ղազախ" "մաղ" "աղ" "պաղպաղակ" "աքաղաղ" "թաղում" "պատառաքաղ" "ղուրան"
     "սկիզբ" "ստրուկ" "զբաղմունք" "գիրք" "կատու" "տղա" "գինի" "գլխարկ" "անտառ" "անուն"
     "ձի" "ձու" "ձուկ" "անձ" "գանձ" "տանձ" "բրինձ" "ձգտում"
     "ծառ" "ծակ" "ծունկ" "ծիտ" "ծիրան" "ծառա" "գիծ" "թախիծ" "ծաղիկ" "ծիծաղ"
     "ջուր" "ջահ" "ջազ" "ջիղ" "ջնջում" "ջութակ" "գաջ" "շրջան" "ջուլհակ"
     "ճաշ" "ճանճ" "ճամփա" "ճիչ" "ճակատ" "ճակատագիր" "ճառ" "ճաք" "ճնշում" "աճ" "լիճ" "ճահիճ"
     "ելք" "եղնիկ" "երազ" "երկինք" "երշիկ" "երաժիշտ" "երակ" "երկաթ"
     "տեր" "կես" "վեպ" "պարտեզ" "պատվեր" "վեճ" "զենք" "նվեր" "բազե" "ափսե"
     "օր" "կեսօր" "աշխօր" "օժիտ" "անօժիտ" "օղակ" "տափօղակ"
     "ող" "որակ" "ոսկի" "ոճ" "ոչինչ" "ոտք"
     "զորք" "օրոր" "մետրո" "գործ" "ծնող" "կով" "հող"
     "ուսանող" "ընկեր" "ուսուցիչ" "հիվանդանոց" "երեխա" "հաց" "տետրը" "խաղողը" "սուրճ" "խնձոր" "միս" "բաժակ"
     "արև" "տերև" "թև" "ձև" "անձրև" "Երևան" "Եվրոպա"
     "սենյակ" "կայան" "հայր" "մայր" "ձայն" "հայ" "ձյուն" "սյուն" "արյուն" "անկյուն" "քույր" "լույս" "հույս" "մայոր"
     "շոյել" "գոյական" "թեյ" "հոկեյ" "հեքիաթ" "կրիա" "խավիար" "օվկիանոս" "սիրտ" "թիվ" "գիծ" "միտք"
     "ամիս" "ամսաթիվ" "երկիր" "երկրագունդ" "պանիր" "պանրիկ" "պատիվ" "պատվոգիր" "երգիչ" "երգչուհի")
   :test #'equal))

(defparameter *verbs*
  (remove-duplicates
   '("գնալ" "գնել" "ունել" "արել" "աղալ" "խաղալ" "ստանալ" "զգալ" "գրել" "եռալ" "վիճել" "զինել"
     "ուտել" "երագել" "որսալ" "սովորել" "որոշել" "գոռալ" "գովել" "հայհոյել" "նայել" "ծառայել"
     "գծել" "մտածել")
   :test #'equal))

(defparameter *adverbs*
  (remove-duplicates
   '("հիմա" "զգաստ" "ցած" "ծածուկ" "գրեթե" "մոտ" "հետո" "նորից" "այժմ ևեթ" "իսկույն ևեթ" "արդյոք")
   :test #'equal))
