%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

s(s(SEN)) --> sentence(SEN).
s(s(SEN,C_SEN)) --> sentence(SEN), conjunct_sentence(C_SEN).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sentence(sen(N_P,V_P)) --> noun_phrase(N_P), verb_phrase(V_P).
sentence(sen(N_P,V_P,PRO_P)) --> noun_phrase(N_P), verb_phrase(V_P), pronoun_phrase(PRO_P).
sentence(sen(what,V,N_P,V1)) --> [what], verb_did(V), noun_phrase(N_P), verb_do(V1).
sentence(sen(what,V,N_P,V1,PREP_P)) --> [what], verb_did(V), noun_phrase(N_P), verb_do(V1), preposition_phrase(PREP_P).
sentence(sen(what,V,N_P,ADV_P,V1)) --> [what], verb_did(V), noun_phrase(N_P), adverb_phrase(ADV_P), verb_do(V1).
sentence(sen(what,V,N_P,ADV_P,V1,PREP_P)) --> [what], verb_did(V), noun_phrase(N_P), adverb_phrase(ADV_P), verb_do(V1), preposition_phrase(PREP_P).
sentence(sen(what,V,N_P,V1,ADV_P)) --> [what], verb_did(V), noun_phrase(N_P), verb_do(V1), adverb_phrase(ADV_P).
sentence(sen(what,V,N_P,V1,ADV_P,PREP_P)) --> [what], verb_did(V), noun_phrase(N_P), verb_do(V1), adverb_phrase(ADV_P), preposition_phrase(PREP_P).
sentence(sen(what,V,N_P,V1,PREP_P,ADV_P)) --> [what], verb_did(V), noun_phrase(N_P), verb_do(V1), preposition_phrase(PREP_P), adverb_phrase(ADV_P).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

conjunct_sentence(c_sen(CON,SEN)) --> conjunction(CON), sentence(SEN).
conjunct_sentence(c_sen(CON,SEN,C_SEN)) --> conjunction(CON), sentence(SEN), conjunct_sentence(C_SEN).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

noun_phrase(n_p(D,N)) --> single_det(D), single_noun(N).
noun_phrase(n_p(D,N,CON,N_P)) --> single_det(D), single_noun(N), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(D,ADJ,N)) --> single_det(D), adjective_phrase(ADJ), single_noun(N).
noun_phrase(n_p(D,ADJ,N,CON,N_P)) --> single_det(D), adjective_phrase(ADJ), single_noun(N), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(D,N)) --> neutral_det(D), single_noun(N).
noun_phrase(n_p(D,N,CON,N_P)) --> neutral_det(D), single_noun(N), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(D,ADJ,N)) --> neutral_det(D), adjective_phrase(ADJ), single_noun(N).
noun_phrase(n_p(D,ADJ,N,CON,N_P)) --> neutral_det(D), adjective_phrase(ADJ), single_noun(N), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(D,N)) --> neutral_det(D), plural_noun(N).
noun_phrase(n_p(D,N,CON,N_P)) --> neutral_det(D), plural_noun(N), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(D,ADJ,N)) --> neutral_det(D), adjective_phrase(ADJ), plural_noun(N).
noun_phrase(n_p(D,ADJ,N,CON,N_P)) --> neutral_det(D), adjective_phrase(ADJ), plural_noun(N), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(D,N)) --> plural_det(D), plural_noun(N).
noun_phrase(n_p(D,N,CON,N_P)) --> plural_det(D), plural_noun(N), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(D,ADJ,N)) --> plural_det(D), adjective_phrase(ADJ), plural_noun(N).
noun_phrase(n_p(D,ADJ,N,CON,N_P)) --> plural_det(D), adjective_phrase(ADJ), plural_noun(N), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(S_PRO)) --> subject_pronoun(S_PRO).
noun_phrase(n_p(S_PRO,CON,N_P)) --> subject_pronoun(S_PRO), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(N)) --> plural_noun(N).
noun_phrase(n_p(N,CON,N_P)) --> plural_noun(N), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(ADJ,N)) --> adjective_phrase(ADJ), plural_noun(N).
noun_phrase(n_p(ADJ,N,CON,N_P)) --> adjective_phrase(ADJ), plural_noun(N), conjunction(CON), noun_phrase(N_P).
noun_phrase(n_p(I_PRO)) --> interrogative_pronoun(I_PRO).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

verb_phrase(v_p(VS,N_P)) --> verbs(VS), noun_phrase(N_P).
verb_phrase(v_p(VS,N_P,CON,V_P)) --> verbs(VS), noun_phrase(N_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(ADV_P,VS,N_P)) --> adverb_phrase(ADV_P), verbs(VS), noun_phrase(N_P).
verb_phrase(v_p(ADV_P,VS,N_P,CON,V_P)) --> adverb_phrase(ADV_P), verbs(VS), noun_phrase(N_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(VS,N_P,ADV_P)) --> verbs(VS), noun_phrase(N_P), adverb_phrase(ADV_P).
verb_phrase(v_p(VS,N_P,ADV_P,CON,V_P)) --> verbs(VS), noun_phrase(N_P), adverb_phrase(ADV_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(VS,N_P,PREP_P)) --> verbs(VS), noun_phrase(N_P), preposition_phrase(PREP_P).
verb_phrase(v_p(VS,N_P,PREP_P,CON,V_P)) --> verbs(VS), noun_phrase(N_P), preposition_phrase(PREP_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(ADV_P,VS,N_P,PREP_P)) --> adverb_phrase(ADV_P), verbs(VS), noun_phrase(N_P), preposition_phrase(PREP_P).
verb_phrase(v_p(ADV_P,VS,N_P,PREP_P,CON,V_P)) --> adverb_phrase(ADV_P), verbs(VS), noun_phrase(N_P), preposition_phrase(PREP_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(VS,N_P,ADV_P,PREP_P)) --> verbs(VS), noun_phrase(N_P), adverb_phrase(ADV_P), preposition_phrase(PREP_P).
verb_phrase(v_p(VS,N_P,ADV_P,PREP_P,CON,V_P)) --> verbs(VS), noun_phrase(N_P), adverb_phrase(ADV_P), preposition_phrase(PREP_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(VS,N_P,PREP_P,ADV_P)) --> verbs(VS), noun_phrase(N_P), preposition_phrase(PREP_P), adverb_phrase(ADV_P).
verb_phrase(v_p(VS,N_P,PREP_P,ADV_P,CON,V_P)) --> verbs(VS), noun_phrase(N_P), preposition_phrase(PREP_P), adverb_phrase(ADV_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(VS_G,N_P,N_P1)) --> verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1).
verb_phrase(v_p(VS_G,N_P,N_P1,CON,V_P)) --> verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(ADV_P,VS_G,N_P,N_P1)) --> adverb_phrase(ADV_P), verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1).
verb_phrase(v_p(ADV_P,VS_G,N_P,N_P1,CON,V_P)) --> adverb_phrase(ADV_P), verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(VS_G,N_P,N_P1,ADV_P)) --> verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), adverb_phrase(ADV_P).
verb_phrase(v_p(VS_G,N_P,N_P1,ADV_P,CON,V_P)) --> verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), adverb_phrase(ADV_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(VS_G,N_P,N_P1,PREP_P)) --> verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), preposition_phrase(PREP_P).
verb_phrase(v_p(VS_G,N_P,N_P1,PREP_P,CON,V_P)) --> verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), preposition_phrase(PREP_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(ADV_P,VS_G,N_P,N_P1,PREP_P)) --> adverb_phrase(ADV_P), verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), preposition_phrase(PREP_P).
verb_phrase(v_p(ADV_P,VS_G,N_P,N_P1,PREP_P,CON,V_P)) --> adverb_phrase(ADV_P), verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), preposition_phrase(PREP_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(VS_G,N_P,N_P1,ADV_P,PREP_P)) --> verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), adverb_phrase(ADV_P), preposition_phrase(PREP_P).
verb_phrase(v_p(VS_G,N_P,N_P1,ADV_P,PREP_P,CON,V_P)) --> verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), adverb_phrase(ADV_P), preposition_phrase(PREP_P), conjunction(CON), verb_phrase(V_P).
verb_phrase(v_p(VS_G,N_P,N_P1,PREP_P,ADV_P)) --> verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), preposition_phrase(PREP_P), adverb_phrase(ADV_P).
verb_phrase(v_p(VS_G,N_P,N_P1,PREP_P,ADV_P,CON,V_P)) --> verbs_gave(VS_G), noun_phrase(N_P), noun_phrase(N_P1), preposition_phrase(PREP_P), adverb_phrase(ADV_P), conjunction(CON), verb_phrase(V_P).


verbs(vs(V)) --> verb(V).
verbs(vs(V,CON,VS)) --> verb(V), conjunction(CON), verbs(VS).
verbs(vs(V)) --> verb_did(V).
verbs(vs(V,CON,VS)) --> verb_did(V), conjunction(CON), verbs(VS).

verbs_gave(vs_g(V)) --> verb_gave(V).
verbs_gave(vs_g(VS,CON,V)) --> verbs(VS), conjunction(CON), verb_gave(V).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

adverb_phrase(adv_p(ADV)) --> adverb(ADV).
adverb_phrase(adv_p(ADV,CON,ADV_P)) --> adverb(ADV), conjunction(CON), adverb_phrase(ADV_P).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

adjective_phrase(adj_p(ADJ)) --> adjective(ADJ).
adjective_phrase(adj_p(ADJ,ADJ_P)) --> adjective(ADJ), adjective_phrase(ADJ_P).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

preposition_phrase(prep_p(PREP,N)) --> preposition(PREP), single_noun(N).
preposition_phrase(prep_p(PREP,N,PREP_P)) --> preposition(PREP), single_noun(N), preposition_phrase(PREP_P).
preposition_phrase(prep_p(PREP,N)) --> preposition(PREP), plural_noun(N).
preposition_phrase(prep_p(PREP,N,PREP_P)) --> preposition(PREP), plural_noun(N), preposition_phrase(PREP_P).
preposition_phrase(prep_p(PREP,N_P)) --> preposition(PREP), noun_phrase(N_P).
preposition_phrase(prep_p(PREP,N_P,PREP_P)) --> preposition(PREP), noun_phrase(N_P), preposition_phrase(PREP_P).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pronoun_phrase(pro_p(O_PRO,N_P,VS)) --> object_pronoun(O_PRO), noun_phrase(N_P), verbs(VS).
pronoun_phrase(pro_p(O_PRO,N_P,VS_G,N_P1)) --> object_pronoun(O_PRO), noun_phrase(N_P), verbs_gave(VS_G), noun_phrase(N_P1).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 5 determiners

single_det(d(a)) --> [a].
single_det(d(every)) --> [every].

neutral_det(d(the)) --> [the].
neutral_det(d(some)) --> [some].

plural_det(d(many)) --> [many].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 25 nouns = 20 single nouns + 5 plural nouns

single_noun(n(boy)) --> [boy].
single_noun(n(box)) --> [box].
single_noun(n(room)) --> [room].
single_noun(n(school)) --> [school].
single_noun(n(woman)) --> [woman].
single_noun(n(man)) --> [man].
single_noun(n(envelope)) --> [envelope].
single_noun(n(shed)) --> [shed].
single_noun(n(building)) --> [building].
single_noun(n(tree)) --> [tree].
single_noun(n(girl)) --> [girl].
single_noun(n(flower)) --> [flower].
single_noun(n(telescope)) --> [telescope].
single_noun(n(university)) --> [university].
single_noun(n(institution)) --> [institution].
single_noun(n(student)) --> [student].
single_noun(n(professor)) --> [professor].
single_noun(n(lecturer)) --> [lecturer].
single_noun(n(scientist)) --> [scientist].
single_noun(n(researcher)) --> [researcher].

plural_noun(n(students)) --> [students].
plural_noun(n(professors)) --> [professors].
plural_noun(n(lecturers)) --> [lecturers].
plural_noun(n(scientists)) --> [scientists].
plural_noun(n(researchers)) --> [researchers].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%20 verbs

verb(v(pushed)) --> [pushed].
verb(v(stored)) --> [stored].
verb(v(climbed)) --> [climbed].
verb(v(watched)) --> [watched].
verb(v(liked)) --> [liked].
verb(v(admired)) --> [admired].
verb(v(appreciated)) --> [appreciated].
verb(v(jumped)) --> [jumped].
verb(v(played)) --> [played].
verb(v(replaced)) --> [replaced].
verb(v(hated)) --> [hated].
verb(v(killed)) --> [killed].
verb(v(worked)) --> [worked].
verb(v(saw)) --> [saw].
verb(v(found)) --> [found].
verb(v(loved)) --> [loved].
verb(v(ran)) --> [ran].
verb_gave(v(gave)) --> [gave].
verb_did(v(did)) --> [did].
verb_do(v(do)) --> [do].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 20 adjectives

adjective(adj(young)) --> [young].
adjective(adj(big)) --> [big].
adjective(adj(large)) --> [large].
adjective(adj(empty)) --> [empty].
adjective(adj(old)) --> [old].
adjective(adj(poor)) --> [poor].
adjective(adj(white)) --> [white].
adjective(adj(brilliant)) --> [brilliant].
adjective(adj(talented)) --> [talented].
adjective(adj(bright)) --> [bright].
adjective(adj(gentle)) --> [gentle].
adjective(adj(fierce)) --> [fierce].
adjective(adj(ambitious)) --> [ambitious].
adjective(adj(beautiful)) --> [beautiful].
adjective(adj(exciting)) --> [exciting].
adjective(adj(rich)) --> [rich].
adjective(adj(happy)) --> [happy].
adjective(adj(sad)) --> [sad].
adjective(adj(calm)) --> [calm].
adjective(adj(nervous)) --> [nervous].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 10 adverbs

adverb(adv(quickly)) --> [quickly].
adverb(adv(together)) --> [together].
adverb(adv(innocently)) --> [innocently].
adverb(adv(always)) --> [always].
adverb(adv(strictly)) --> [strictly].
adverb(adv(likely)) --> [likely].
adverb(adv(faithfully)) --> [faithfully].
adverb(adv(carelessly)) --> [carelessly].
adverb(adv(secretly)) --> [secretly].
adverb(adv(often)) --> [often].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 10 prepositions

preposition(prep(in)) --> [in].
preposition(prep(after)) --> [after].
preposition(prep(behind)) --> [behind].
preposition(prep(under)) --> [under].
preposition(prep(around)) --> [around].
preposition(prep(before)) --> [before].
preposition(prep(at)) --> [at].
preposition(prep(above)) --> [above].
preposition(prep(within)) --> [within].
preposition(prep(below)) --> [below].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

subject_pronoun(s_pro(she)) --> [she].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

object_pronoun(o_pro(whom)) --> [whom].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

interrogative_pronoun(i_pro(who)) --> [who].
interrogative_pronoun(i_pro(what)) --> [what].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

conjunction(con(and)) --> [and].
