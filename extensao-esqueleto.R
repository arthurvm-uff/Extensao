# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc

dados_sinasc=read.csv("SINASC_2015.csv", header = TRUE, sep = ";")

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

dados_sinasc_1=subset(dados_sinasc, select = c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61))

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

dados_sinasc_2 <- dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "13", ]

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 

table(substr(dados_sinasc_2$CODMUNRES, 1, 2))

# Exportar o arquivo com o nome dados_sinasc_2.csv

write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = TRUE)

# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)

# Aproveitando para ver os valores das variáveis quantitativas
unique(dados_sinasc_2$IDADEMAE)
unique(dados_sinasc_2$CONSPRENAT)
unique(dados_sinasc_2$SEMAGESTAC)
unique(dados_sinasc_2$APGAR5)
unique(dados_sinasc_2$PESO)
summary(dados_sinasc_2$PESO)

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável

dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA
summary(dados_sinasc_2)

# Por curiosidade, verificando o tamanho dos banco de dados referente ao estado e aos municípios com e sem NAs
n_total_nasc_UF = nrow(dados_sinasc_2)
n_total_nasc_UF_sem_missing = sum(complete.cases(dados_sinasc_2))
n_total_nasc_MUN = tapply(rep(1, nrow(dados_sinasc_2)), dados_sinasc_2$CODMUNRES, sum)
n_total_nasc_MUN_sem_missing = tapply(complete.cases(dados_sinasc_2), dados_sinasc_2$CODMUNRES, sum)

# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4,5), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros", "Aldeia indígena"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "32 a 36 semanas", "42 semanas e mais"))
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3), labels = c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1,2), labels = c("Vaginal", "Cesário"))
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1,2), labels = c("Sim", "Não"))
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5), labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3), labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(0,1), labels = c("Nulípara", "Multípara"))
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))

# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

# criar nova variável referente ao deslocamento materno para realizar o parto, chamado de peregrinação
# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES

dados_sinasc_2$F_IDADE = ifelse(dados_sinasc_2$IDADEMAE < 15, "<15",
                                ifelse(dados_sinasc_2$IDADEMAE <= 19, "15-19",
                                       ifelse(dados_sinasc_2$IDADEMAE <= 24, "20-24",
                                              ifelse(dados_sinasc_2$IDADEMAE <= 29, "25-29",
                                                     ifelse(dados_sinasc_2$IDADEMAE <= 34, "30-34",
                                                            ifelse(dados_sinasc_2$IDADEMAE <= 39, "35-39",
                                                                   ifelse(dados_sinasc_2$IDADEMAE <= 44, "40-44",
                                                                          ifelse(dados_sinasc_2$IDADEMAE <= 49, "45-49",
                                                                                 "50+"))))))))
dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE,
                                levels = c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"), ordered = TRUE)

dados_sinasc_2$F_PESO = ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso",
                               ifelse(dados_sinasc_2$PESO < 4000, "Peso normal",
                                      "Macrossomia"))
dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia"))

dados_sinasc_2$F_APGAR5 = ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5,levels = c("Baixo","Normal"))

dados_sinasc_2$PERIG = ifelse(is.na(dados_sinasc_2$CODMUNNASC) | is.na(dados_sinasc_2$CODMUNRES), NA,
                              ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$PERIG = factor(dados_sinasc_2$PERIG, levels = c("Não", "Sim"))

dados_sinasc_2$ESTCIV = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
                               ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA))
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro"))

# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a
#partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme
# indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única:
# dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90,
#GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que
# em dados_sinasc_2 SEXO está como fator com as categorias Masculino e Feminino.

tabela_pig = read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep=";")
tabela_pig$SEXO = factor(tabela_pig$SEXO, levels = c("Masculino", "Feminino"))
dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC","SEXO"), all.x = TRUE)
dados_sinasc_2$F_PIG=ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,
                            ifelse(is.na(dados_sinasc_2$PESO)|is.na(dados_sinasc_2$PESO_P10)|is.na(dados_sinasc_2$PESO_P90),
                                   NA,
                                   ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                                          ifelse(dados_sinasc_2$PESO<=dados_sinasc_2$PESO_P90, "AIG", "GIG"))))
dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG","AIG","GIG"))

# Tarefas 9 e 10 (reformulada)
#Crie um banco de dados contendo as 103 variáveis listadas no arquivo
#“Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf”
#O banco final deverá possuir:
#  • 103 colunas, correspondentes às variáveis especificadas;
#• n + 1 linhas, onde:
#  • n corresponde ao número de municípios distintos da UF em análise
#• a primeira linha corresponde aos valores agregados para a UF como
#um todo;
#• as demais linhas correspondem aos municípios da UF.
#As variáveis devem ser construídas a partir dos microdados do SINASC, respeitando os nomes e a ordem especificados.

base = data.frame(CODMUNRES=sort(unique(dados_sinasc_2$CODMUNRES)))
length(base)
#ANO:
base = cbind(ANO = 2015, base)

#TN - Total de nascimentos:
TN = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES, levels=base$CODMUNRES)))
names(TN) = c("CODMUNRES","TN")
base = merge(base,TN,by="CODMUNRES", all.x=TRUE)

#TNRC - Total de nascimentos com registros completos:
dados_UF = dados_sinasc[substr(as.character(dados_sinasc$CODMUNRES),1,2)=="13",]
dados_UF_comp = dados_UF[complete.cases(dados_UF),]
TNRC = as.data.frame(table(factor(dados_UF_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRC) = c("CODMUNRES","TNRC")
base = merge(base,TNRC,by="CODMUNRES",all.x = TRUE)

#TNRCR - Total de nascimentos com dados completos nas 22 variaveis
dados_UF_1 = dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "13",]
dados_UF_1_comp = dados_UF_1[complete.cases(dados_UF_1), ]
TNRCR = as.data.frame(table(factor(dados_UF_1_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRCR) = c("CODMUNRES","TNRCR")
base = merge(base, TNRCR, by = "CODMUNRES", all.x = TRUE)

#TGI - Total de gestantes por idade:
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_IDADE, levels = c("<15","15-19","20-24","25-29", "30-34","35-39","40-44","45-49","50+")))
df = as.data.frame.matrix(tab)
names(df) = c("TGI_15","TGI_15_19","TGI_20_24","TGI_25_29", "TGI_30_34","TGI_35_39","TGI_40_44","TGI_45_49","TGI_50")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

#TGIF - Total de gestantes em idade fértil:
anos_filtrados = dados_sinasc_2[dados_sinasc_2$F_IDADE %in% c("15-19", "20-24","25-29","30-34","35-39","40-44","45-49"),]
gif = table(anos_filtrados$CODMUNRES)
TGIF = as.data.frame(gif)
names(TGIF) = c("CODMUNRES","TGIF")
base = merge(base, TGIF, by = "CODMUNRES", all.x = TRUE)

#IM_P - Percentis das idades maternas:
tmp = aggregate(IDADEMAE ~ CODMUNRES,data = dados_sinasc_2,FUN = function(x) c(p25 = quantile(x, 0.25, na.rm = TRUE),p50 = quantile(x, 0.50, na.rm = TRUE),p75 = quantile(x, 0.75, na.rm = TRUE),media = mean(x, na.rm = TRUE),sd = sd(x, na.rm = TRUE)))
tmp_resumo = data.frame(CODMUNRES = tmp$CODMUNRES,tmp$IDADEMAE)
names(tmp_resumo) = c("CODMUNRES","IM_P25","IM_P50","IM_P75","IM_MD","IM_DP")
base = merge(base, tmp_resumo, by = "CODMUNRES", all.x = TRUE)

#EM - Total de gestantes por nível de escolaridade:
EM = as.data.frame.matrix(table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$ESCMAE2010))
names(EM) = c("EM_S","EM_FI","EM_FII","EM_M","EM_SI","EM_SC")
EM$CODMUNRES = rownames(EM)
base = merge(base,EM, by="CODMUNRES", all.x=TRUE)

#TGRC - Total de gestantes por raça/cor:
TGRC = as.data.frame.matrix(table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$RACACORMAE))
names(TGRC) = c("TGRC_B","TGRC_PT","TGRC_A","TGRC_PD","TGRC_I")
TGRC$CODMUNRES = rownames(TGRC)
base = merge(base,TGRC, by="CODMUNRES", all.x=TRUE)

#TGSC e TGCC - Total de gestantes com/sem companheiro:
parceiro = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"),"TGCC","TGSC")
tab_parceiro = table(dados_sinasc_2$CODMUNRES, parceiro)
df_estado_civil = as.data.frame.matrix(tab_parceiro)
df_estado_civil$CODMUNRES = rownames(df_estado_civil)
base = merge(base, df_estado_civil, by="CODMUNRES", all.x = TRUE)

#TGPRI e TGNPRI - Total de gestantes primíparas ou não:
tab_pri = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$PARIDADE)
df_pri = as.data.frame.matrix(tab_pri)
names(df_pri) = c("TGPRI","TGNPRI")
df_pri$CODMUNRES = rownames(df_pri)
base = merge(base, df_pri, by="CODMUNRES", all.x = TRUE)

#TGU e TGG - Total de gestações únicas ou gemelares:
grav = ifelse(dados_sinasc_2$GRAVIDEZ %in% c("Única"), "Única", "Gemelar")
tab_grav = table(dados_sinasc_2$CODMUNRES, grav)
df_gravidez = as.data.frame.matrix(tab_grav)
names(df_gravidez) = c("TGU","TGG")
df_gravidez$CODMUNRES = rownames(df_gravidez)
base = merge(base,df_gravidez,by="CODMUNRES", all.x=TRUE)

#TGD - Total de gestações por duração:
tab_temp_grav = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$GESTACAO, levels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "41 semanas e mais")))
df_temp_grav = as.data.frame.matrix(tab_temp_grav)
names(df_temp_grav) = c("TGD_22","TGD_22_27","TGD_28_31","TGD_32_36", "TGD_37_41","TGD_42")
df_temp_grav$CODMUNRES = rownames(df_temp_grav)
base = merge(base, df_temp_grav, by = "CODMUNRES", all.x = TRUE)

#TGD_PRT, TGD_AT, TGD_PST - Total de gestações pré-termo/a termo/pós-termo:
cat_gest = ifelse(dados_sinasc_2$GESTACAO %in% c("Menos de 22 semanas"), "TGD_PRT", ifelse(dados_sinasc_2$GESTACAO %in% c("22 a 27 semanas"), "TGD_PRT", ifelse(dados_sinasc_2$GESTACAO %in% c("28 a 31 semanas"), "TGD_PRT", ifelse(dados_sinasc_2$GESTACAO %in% c("32 a 36 semanas"), "TGD_PRT", ifelse(dados_sinasc_2$GESTACAO %in% c("37 a 41 semanas"), "TGD_AT", "TGD_PST")))))
cat_gest = factor(cat_gest, levels = c("TGD_PRT","TGD_AT","TGD_PST"))
tab_cat_gest = table(dados_sinasc_2$CODMUNRES, cat_gest)
df_cat_gest = as.data.frame.matrix(tab_cat_gest)
names(df_cat_gest) = c("TGD_PRT","TGD_AT","TGD_PST")
df_cat_gest$CODMUNRES = rownames(df_cat_gest)
base = merge(base, df_cat_gest, by="CODMUNRES",all.x = TRUE)

#DG - Percentis, média e desvio-padrão da duração das gestações:
tdg = aggregate(SEMAGESTAC ~ CODMUNRES,data = dados_sinasc_2,FUN = function(x) c(p25 = quantile(x, 0.25, na.rm = TRUE),p50 = quantile(x, 0.50, na.rm = TRUE),p75 = quantile(x, 0.75, na.rm = TRUE),media = mean(x, na.rm = TRUE),sd = sd(x, na.rm = TRUE)))
tdg_resumo = data.frame(CODMUNRES = tdg$CODMUNRES,tdg$SEMAGESTAC)
names(tdg_resumo) = c("CODMUNRES","DG_P25","DG_P50","DG_P75","DG_MD","DG_DP")
base = merge(base, tdg_resumo, by = "CODMUNRES", all.x = TRUE)

#TKC - Total de consultas por qualidade do pré-natal:
tab_tkc = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$KOTELCHUCK)
df_tkc = as.data.frame.matrix(tab_tkc)
names(df_tkc) = c("TKC_NR","TKC_ID","TKC_IT","TKC_AD","TKC_MAD")
df_tkc$CODMUNRES = rownames(df_tkc)
base = merge(base, df_tkc, by = "CODMUNRES", all.x = TRUE)

#TGPRG - Total de gestantes por avaliação de peregrinação:
peregrinou = ifelse(dados_sinasc_2$CODMUNRES != dados_sinasc_2$CODMUNNASC, "Peregrinou", "Não peregrinou")
tab_tgprg = table(dados_sinasc_2$CODMUNRES, peregrinou)
df_tgprg = as.data.frame.matrix(tab_tgprg)
names(df_tgprg) = c("TGPRG_S","TGPRG_N")
df_tgprg$CODMUNRES = rownames(df_tgprg)
base = merge(base, df_tgprg, by="CODMUNRES",all.x = TRUE)

#TP - Total de partos por classificação entre cesáreo/vaginal:
tab_tp = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$PARTO)
df_tp = as.data.frame.matrix(tab_tp)
names(df_tp) = c("TPV","TPC")
df_tp$CODMUNRES = rownames(df_tp)
base = merge(base,df_tp,by="CODMUNRES",all.x = TRUE)

#TRAP - Total de recém nascidos por posição:
tab_trap = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$TPAPRESENT)
df_trap = as.data.frame.matrix(tab_trap)
names(df_trap) = c("TRAP_C","TRAP_P","TRAP_T")
df_trap$CODMUNRES = rownames(df_trap)
base = merge(base,df_trap,by="CODMUNRES",all.x = TRUE)

#TGROB - Total de gestantes por grupo de Robson:
tab_TGROB = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$TPROBSON)
df_TGROB = as.data.frame.matrix(tab_TGROB)
names(df_TGROB) = c("TGROB_1","TGROB_2","TGROB_3","TGROB_4","TGROB_5","TGROB_6","TGROB_7","TGROB_8","TGROB_9","TGROB_10")
df_TGROB$CODMUNRES = rownames(df_TGROB)
base = merge(base,df_TGROB,by="CODMUNRES",all.x=TRUE)

#TNLOC - Total de nascimentos por localidade:
tab_TNLOC = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$LOCNASC)
df_TNLOC = as.data.frame.matrix(tab_TNLOC)
names(df_TNLOC) = c("TNLOC_H","TNLOC_ES","TNLOC_D","TNLOC_O","TNLOC_AI")
df_TNLOC$CODMUNRES = rownames(df_TNLOC)
base = merge(base,df_TNLOC,by="CODMUNRES",all.x=TRUE)

#TRS - Total de recém-nascidos por sexo:
tab_TRS = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$SEXO)
df_trs = as.data.frame.matrix(tab_TRS)
names(df_trs) = c("TRS_M","TRS_F")
df_trs$CODMUNRES = rownames(df_trs)
base = merge(base,df_trs,by="CODMUNRES",all.x=TRUE)

#TRRC - Total de recém nascidos por raça-cor:
tab_trrc = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$RACACOR)
df_trrc = as.data.frame.matrix(tab_trrc)
names(df_trrc) = c("TRRC_B","TRRC_PT","TRRC_A","TRRC_PD","TRRC_I")
df_trrc$CODMUNRES = rownames(df_trrc)
base = merge(base,df_trrc,by="CODMUNRES",all.x=TRUE)

#TRP - Total de recém nascidos por peso:
tab_trp = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$F_PESO)
df_trp = as.data.frame.matrix(tab_trp)
names(df_trp) = c("TRP_BP","TRP_N","TRP_M")
df_trp$CODMUNRES = rownames(df_trp)
base = merge(base,df_trp,by="CODMUNRES",all.x=TRUE)

#PESO - Percentis do peso dos recém-nascidos:
pes = aggregate(PESO ~ CODMUNRES,data = dados_sinasc_2,FUN = function(x) c(p25 = quantile(x, 0.25, na.rm = TRUE),p50 = quantile(x, 0.50, na.rm = TRUE),p75 = quantile(x, 0.75, na.rm = TRUE),media = mean(x, na.rm = TRUE),sd = sd(x, na.rm = TRUE)))
pes_resumo = data.frame(CODMUNRES = pes$CODMUNRES,pes$PESO)
names(pes_resumo) = c("CODMUNRES","PESO_P25","PESO_P50","PESO_P75","PESO_MD","PESO_DP")
base = merge(base, pes_resumo, by = "CODMUNRES", all.x = TRUE)

#TRPIG - PIG dos recém-nascidos de gestações únicas:
tab_pig = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$F_PIG) 
df_pig = as.data.frame.matrix(tab_pig) 
names(df_pig) = c("TRPIG_P","TRPIG_A","TRPIG_G") 
df_pig$CODMUNRES = rownames(df_pig) 
base = merge(base,df_pig,by="CODMUNRES",all.x=TRUE)

#TRAPG5 - Total de recém nascidos por valor do APGAR5:
tip_apgar5 = ifelse(dados_sinasc_2$APGAR5 < 7, "TRAPG5_B", "TRAPG5_N")
tab_apgar5 = table(dados_sinasc_2$CODMUNRES, tip_apgar5)
df_apgar5 = as.data.frame.matrix(tab_apgar5)
names(df_apgar5) = c("TRAPG5_B","TRAPG5_N")
df_apgar5$CODMUNRES = rownames(df_apgar5)
base = merge(base,df_apgar5,by="CODMUNRES",all.x=TRUE)

#APG5 - Média e desvio-padrão do APGAR5 dos recém-nascidos:
apg5 = aggregate(APGAR5 ~ CODMUNRES,data = dados_sinasc_2, FUN = function(x) c(mean = mean(x, na.rm = TRUE),sd = sd(x, na.rm = TRUE)))

apg5_resumo = data.frame(CODMUNRES = apg5$CODMUNRES,APG5_MD   = apg5$APGAR5[, "mean"],APG5_DP   = apg5$APGAR5[, "sd"])

base = merge(base, apg5_resumo, by = "CODMUNRES", all.x = TRUE)

#TRA - Total de recém-nascidos com/sem anomalia congênita:
tab_tra = table(dados_sinasc_2$CODMUNRES, dados_sinasc_2$IDANOMAL)
df_tra = as.data.frame.matrix(tab_tra)
names(df_tra) = c("TRAC","TRSAC")
df_tra$CODMUNRES = rownames(df_tra)
base = merge(base,df_tra,by="CODMUNRES",all.x = TRUE)

#Agora, adicionando o total de todos com o nivel UF
cols_contagem = setdiff(names(base), c("CODMUNRES","ANO","TN","TNRC","TNRCR","TGI_15","TGI_15_19","TGI_20_24","TGI_25_29","TGI_30_34","TGI_35_39","TGI_40_44","TGI_45_49","TGI_50","TGIF","IM_P25","IM_P50","IM_P75","IM_MD","IM_DP","EM_S","EM_FI","EM_FII","EM_M","EM_SI","EM_SC","TGRC_B","TGRC_PT","TGRC_A","TGRC_PD","TGRC_I","TGCC","TGSC","TGPRI","TGNPRI","TGU","TGG","TGD_22","TGD_22_27","TGD_28_31","TGD_32_36","TGD_37_41","TGD_42","TGD_PRT","TGD_AT","TGD_PST","DG_P25","DG_P50","DG_P75","DG_MD","DG_DP","TKC_NR","TKC_ID","TKC_IT","TKC_AD","TKC_MAD","TGPRG_S","TGPRG_N","TPV","TPC","TRAP_C","TRAP_P","TRAP_T","TGROB_1","TGROB_2","TGROB_3","TGROB_4","TGROB_5","TGROB_6","TGROB_7","TGROB_8","TGROB_9","TGROB_10","TNLOC_H","TNLOC_ES","TNLOC_D","TNLOC_O","TNLOC_AI","TRS_M","TRS_F","TRRC_B","TRRC_PT","TRRC_A","TRRC_PD","TRRC_I","TRP_BP","TRP_N","TRP_M","PESO_P25","PESO_P50","PESO_P75","PESO_MD","PESO_DP","TRPIG_P","TRPIG_A","TRPIG_G","TRAPG5_B","TRAPG5_N","APG5_MD","APG5_DP","TRAC","TRSAC"))
base[cols_contagem][is.na(base[cols_contagem])] = 0

linha_estado = base[1,]
linha_estado[,] = NA
cols_contagem = setdiff(names(base),c("CODMUNRES","ANO","IM_P25","IM_P50","IM_P75","IM_MD","IM_DP","DG_P25","DG_P50","DG_P75","DG_MD","DG_DP","PESO_P25","PESO_P50","PESO_P75","PESO_MD","PESO_DP","APG5_MD","APG5_DP"))
linha_estado[cols_contagem] = colSums(base[cols_contagem], na.rm = TRUE)

linha_estado$IM_MD = round(mean(dados_sinasc_2$IDADEMAE, na.rm =TRUE),2)
linha_estado$IM_DP = round(sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE),2)
q = round(quantile(dados_sinasc_2$IDADEMAE,probs = c(0.25,0.5,0.75), na.rm = TRUE),2)
linha_estado$IM_P25 = q[1]
linha_estado$IM_P50 = q[2]
linha_estado$IM_P75 = q[3]

linha_estado$CODMUNRES  = 13
SINASC_AM = rbind(linha_estado, base)
SINASC_AM$NIVEL = c("UF", rep("MUNICIPIO",nrow(SINASC_AM)-1))
SINASC_AM$ANO = 2015
SINASC_AM = SINASC_AM[,c("ANO","NIVEL","CODMUNRES", names(SINASC_AM)[!names(SINASC_AM) %in% c("ANO","NIVEL","CODMUNRES")])]

# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv

write.csv(SINASC_AM, "SINASC_AM.csv")

# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commite "Esqueleto atualizado na Etapa 2"
# A partir de main crie a branch SIM
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2
# Para realizar as tarefas da ETAPA 2, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim

dados_sim = read.csv("Mortalidade_Geral_2015.csv", header = TRUE, sep = ";")

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, 
# OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

dados_sim_1=subset(dados_sim, select = c(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84))

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

dados_sim_2 <- dados_sim_1[substr(as.character(dados_sim_1$CODMUNRES), 1, 2) == "13", ]

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645     
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

# Exportar o arquivo com o nome dados_sim_2.csv

write.csv(dados_sim_2, "dados_sim_2.csv", row.names = TRUE)

# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR, 
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

table(dados_sim_2$TIPOBITO)
table(dados_sim_2$SEXO)
table(dados_sim_2$RACACOR)
table(dados_sim_2$TPMORTEOCO)
table(dados_sim_2$OBITOGRAV)
table(dados_sim_2$OBITOPUERP)
table(dados_sim_2$CAUSABAS)
table(dados_sim_2$TPOBITOCOR)
table(dados_sim_2$MORTEPARTO)
table(dados_sim_2$ESC2010)

# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 99 para NA

dados_sim_2$SEXO[dados_sim_2$SEXO == 0] = NA
dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] = NA
dados_sim_2$OBITOGRAV[dados_sim_2$OBITOGRAV == 9] = NA
dados_sim_2$OBITOPUERP[dados_sim_2$OBITOPUERP == 9] = NA
dados_sim_2$TPOBITOCOR[dados_sim_2$TPOBITOCOR == 9] = NA
dados_sim_2$MORTEPARTO[dados_sim_2$MORTEPARTO == 9] = NA
dados_sim_2$IDADE[dados_sim_2$IDADE == 9] = NA
dados_sim_2$ESC2010[dados_sim_2$ESC2010 == 9] = NA

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), 
# labels = c("Fetal", "Não fetal")

dados_sim_2$SEXO = factor(dados_sim_2$SEXO, levels = c(1, 2), labels = c("Masculino", "Feminino"))
dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1, 2), labels = c("Fetal", "Não fetal"))
dados_sim_2$RACACOR = factor(dados_sim_2$RACACOR, levels = c(1, 2, 3, 4, 5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sim_2$TPMORTEOCO = factor(dados_sim_2$TPMORTEOCO, levels = c(1, 2, 3, 4, 5, 8), labels = c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após o término do parto", "De 43 dias a 1 ano após o término da gestação", "Não ocorreu nestes períodos"))
dados_sim_2$OBITOPUERP = factor(dados_sim_2$OBITOPUERP, levels = c(1, 2, 3), labels = c("Sim, até 42 dias após o parto", "Sim, de 43 dias a 1 ano", "Não"))
dados_sim_2$TPOBITOCOR = factor(dados_sim_2$TPOBITOCOR, levels = c(1, 2, 3, 4, 5), labels = c("Via pública", "Endereço de residência", "Outro domicílio", "Estabelecimento comercial", "Outros"))
dados_sim_2$OBITOGRAV = factor(dados_sim_2$OBITOGRAV, levels = c(1, 2), labels = c("Sim", "Não"))
dados_sim_2$MORTEPARTO = factor(dados_sim_2$MORTEPARTO, levels = c(1, 2, 3), labels = c("Antes", "Durante", "Após"))
dados_sim_2$ESC2010 = factor(dados_sim_2$ESC2010, levels = c(0, 1, 2, 3, 4, 5), labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º Grau)", "Superior incompleto", "Superior completo"))

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados


# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE

base = data.frame(CODMUNRES=sort(unique(dados_sim_2$CODMUNRES)))

#ANO
base = cbind(ANO = 2015, base)

#TO - total de Óbitos
contagem <- table(dados_sim_2$CODMUNRES)
base$TO <- as.numeric(contagem[match(base$CODMUNRES, names(contagem))])

#TORC - total de óbitos com registros completos nas 87 variáveis do SIM
dados_sim$completo <- complete.cases(dados_sim)
contagem_torc <- table(dados_sim$CODMUNRES[dados_sim$completo == TRUE])
base$TORC <- as.numeric(contagem_torc[match(base$CODMUNRES, names(contagem_torc))])

#TORCR - total de óbitos com registros completos (sem qualquer NA) nas 14 variáveis selecionadas do SIM
dados_sim_2$completo <- complete.cases(dados_sim_2)
contagem_torcr <- table(dados_sim_2$CODMUNRES[dados_sim_2$completo == TRUE])
base$TORCR <- as.numeric(contagem_torcr[match(base$CODMUNRES, names(contagem_torcr))])

#TO_NN - total de óbitos não naturais
primeira_letra <- substr(dados_sim_2$CAUSABAS, 1, 1)
dados_sim_2$nao_natural <- primeira_letra %in% c("V", "W", "X", "Y")
contagem_to_nn <- table(dados_sim_2$CODMUNRES[dados_sim_2$nao_natural == TRUE])
base$TO_NN <- as.numeric(contagem_to_nn[match(base$CODMUNRES, names(contagem_to_nn))])

#TO_N - total de óbitos naturais
primeira_letra <- substr(dados_sim_2$CAUSABAS, 1, 1)
dados_sim_2$natural <- !(primeira_letra %in% c("V", "W", "X", "Y"))
contagem_to_n <- table(dados_sim_2$CODMUNRES[dados_sim_2$natural == TRUE])
base$TO_N <- as.numeric(contagem_to_n[match(base$CODMUNRES, names(contagem_to_n))])

#TO_CB_I - total de óbitos por doenças infecciosas ou parasitárias
primeira_letra <- substr(dados_sim_2$CAUSABAS, 1, 1)
dados_sim_2$infecciosa <- primeira_letra %in% c("A", "B")
contagem_toci <- table(dados_sim_2$CODMUNRES[dados_sim_2$infecciosa == TRUE])
base$TO_CB_I <- as.numeric(contagem_toci[match(base$CODMUNRES, names(contagem_toci))])

#TO_CB_N - total de óbitos por neoplasias ou doenças hematológicas
primeira_letra <- substr(dados_sim_2$CAUSABAS, 1, 1)
dados_sim_2$neoplasia_hemato <- primeira_letra %in% c("C", "D")
contagem_tocbn <- table(dados_sim_2$CODMUNRES[dados_sim_2$neoplasia_hemato == TRUE])
base$TO_CB_N <- as.numeric(contagem_tocbn[match(base$CODMUNRES, names(contagem_tocbn))])

#TO_CB_C - total de óbitos por doenças circulatórias
primeira_letra <- substr(dados_sim_2$CAUSABAS, 1, 1)
dados_sim_2$circulatorio <- primeira_letra == "I"
contagem_tocbc <- table(dados_sim_2$CODMUNRES[dados_sim_2$circulatorio == TRUE])
base$TO_CB_C <- as.numeric(contagem_tocbc[match(base$CODMUNRES, names(contagem_tocbc))])

#TO_CB_R - total de óbitos por doenças respiratórias
primeira_letra <- substr(dados_sim_2$CAUSABAS, 1, 1)
dados_sim_2$respiratorio <- primeira_letra == "J"
contagem_tocbr <- table(dados_sim_2$CODMUNRES[dados_sim_2$respiratorio == TRUE])
base$TO_CB_R <- as.numeric(contagem_tocbr[match(base$CODMUNRES, names(contagem_tocbr))])

#TO_CB_O - total de óbitos por outras causas naturais
primeira_letra <- substr(dados_sim_2$CAUSABAS, 1, 1)
letras_mapeadas <- c("A", "B", "C", "D", "I", "J", "V", "W", "X", "Y")
dados_sim_2$outras_causas <- !(primeira_letra %in% letras_mapeadas)
contagem_tocbo <- table(dados_sim_2$CODMUNRES[dados_sim_2$outras_causas == TRUE])
base$TO_CB_O <- as.numeric(contagem_tocbo[match(base$CODMUNRES, names(contagem_tocbo))])

#TO_M - total de óbitos masculinos
dados_sim_2$masculino <- dados_sim_2$SEXO == "Masculino"
contagem_tom <- table(dados_sim_2$CODMUNRES[dados_sim_2$masculino == TRUE])
base$TO_M <- as.numeric(contagem_tom[match(base$CODMUNRES, names(contagem_tom))])

#TO_F - total de óbitos femininos
dados_sim_2$feminino <- dados_sim_2$SEXO == "Feminino"
contagem_tof <- table(dados_sim_2$CODMUNRES[dados_sim_2$feminino == TRUE])
base$TO_F <- as.numeric(contagem_tof[match(base$CODMUNRES, names(contagem_tof))])

#TO_F_IF - total de obitos femininos em idade fértil
is_feminino <- dados_sim_2$SEXO == "Feminino"
is_idade_fertil <- dados_sim_2$IDADE >= 415 & dados_sim_2$IDADE <= 449
dados_sim_2$feminino_if <- is_feminino & is_idade_fertil
contagem_tofif <- table(dados_sim_2$CODMUNRES[dados_sim_2$feminino_if == TRUE])
base$TO_F_IF <- as.numeric(contagem_tofif[match(base$CODMUNRES, names(contagem_tofif))])

#TO_FT - total de óbitos fetais
dados_sim_2$fetal <- dados_sim_2$TIPOBITO == "Fetal"
contagem_toft <- table(dados_sim_2$CODMUNRES[dados_sim_2$fetal == TRUE])
base$TO_FT <- as.numeric(contagem_toft[match(base$CODMUNRES, names(contagem_toft))])

#TO_NT - total de óbitos neonatais
dados_sim_2$neonatal <- (dados_sim_2$IDADE >= 100 & dados_sim_2$IDADE <= 299) | 
  (dados_sim_2$IDADE >= 300 & dados_sim_2$IDADE <= 327)
contagem_tont <- table(dados_sim_2$CODMUNRES[dados_sim_2$neonatal == TRUE])
base$TO_NT <- as.numeric(contagem_tont[match(base$CODMUNRES, names(contagem_tont))])

#TO_NT_P - total de óbitos neonatais precoces
dados_sim_2$neonatal_precoce <- (dados_sim_2$IDADE >= 100 & dados_sim_2$IDADE <= 299) | 
  (dados_sim_2$IDADE >= 300 & dados_sim_2$IDADE <= 306)
contagem_tontp <- table(dados_sim_2$CODMUNRES[dados_sim_2$neonatal_precoce == TRUE])
base$TO_NT_P <- as.numeric(contagem_tontp[match(base$CODMUNRES, names(contagem_tontp))])

#TO_NT_T - total de óbitos neonatais tardios
dados_sim_2$neonatal_tardio <- dados_sim_2$IDADE >= 307 & dados_sim_2$IDADE <= 327
contagem_tontt <- table(dados_sim_2$CODMUNRES[dados_sim_2$neonatal_tardio == TRUE])
base$TO_NT_T <- as.numeric(contagem_tontt[match(base$CODMUNRES, names(contagem_tontt))])

#TO_PNT - total de óbitos pós-neonatal
dados_sim_2$pos_neonatal <- (dados_sim_2$IDADE >= 328 & dados_sim_2$IDADE <= 399) | 
  (dados_sim_2$IDADE >= 401 & dados_sim_2$IDADE <= 411)
contagem_topnt <- table(dados_sim_2$CODMUNRES[dados_sim_2$pos_neonatal == TRUE])
base$TO_PNT <- as.numeric(contagem_topnt[match(base$CODMUNRES, names(contagem_topnt))])

#TO_MT_G - total de óbitos maternos durante a gestação
dados_sim_2$gestacao <- dados_sim_2$MORTEPARTO == "Antes"
contagem_tomtg <- table(dados_sim_2$CODMUNRES[dados_sim_2$gestacao == TRUE])
base$TO_MT_G <- as.numeric(contagem_tomtg[match(base$CODMUNRES, names(contagem_tomtg))])

#TONT_B - total de óbitos neonatais da raça/cor branca
is_neonatal <- (dados_sim_2$IDADE >= 100 & dados_sim_2$IDADE <= 299) | 
  (dados_sim_2$IDADE >= 300 & dados_sim_2$IDADE <= 327)
is_branca <- dados_sim_2$RACACOR == "Branca"
dados_sim_2$neonatal_branca <- is_neonatal & is_branca
contagem_tontb <- table(dados_sim_2$CODMUNRES[dados_sim_2$neonatal_branca == TRUE])
base$TONT_B <- as.numeric(contagem_tontb[match(base$CODMUNRES, names(contagem_tontb))])

#TONT_PT - total de óbitos neonatais da raça/cor preta
is_preta <- dados_sim_2$RACACOR == "Preta"
dados_sim_2$neonatal_preta <- is_neonatal & is_preta
contagem_tontpt <- table(dados_sim_2$CODMUNRES[dados_sim_2$neonatal_preta == TRUE])
base$TONT_PT <- as.numeric(contagem_tontpt[match(base$CODMUNRES, names(contagem_tontpt))])

#TONT_A - total de óbitos neonatais da raça/cor amarela
is_amarela <- dados_sim_2$RACACOR == "Amarela"
dados_sim_2$neonatal_amarela <- is_neonatal & is_amarela
contagem_tonta <- table(dados_sim_2$CODMUNRES[dados_sim_2$neonatal_amarela == TRUE])
base$TONT_A <- as.numeric(contagem_tonta[match(base$CODMUNRES, names(contagem_tonta))])

#TONT_PD - total de óbitos neonatais da raça/cor parda
is_parda <- dados_sim_2$RACACOR == "Parda"
dados_sim_2$neonatal_parda <- is_neonatal & is_parda
contagem_tontpd <- table(dados_sim_2$CODMUNRES[dados_sim_2$neonatal_parda == TRUE])
base$TONT_PD <- as.numeric(contagem_tontpd[match(base$CODMUNRES, names(contagem_tontpd))])

#TONT_I - total de óbitos neonatais da raça/cor indígena
is_indigena <- dados_sim_2$RACACOR == "Indígena"
dados_sim_2$neonatal_indigena <- is_neonatal & is_indigena
contagem_tonti <- table(dados_sim_2$CODMUNRES[dados_sim_2$neonatal_indigena == TRUE])
base$TONT_I <- as.numeric(contagem_tonti[match(base$CODMUNRES, names(contagem_tonti))])

#TO_MT - total de óbitos maternos (precoces e tardios)
dados_sim_2$materno_consolidado <- (dados_sim_2$OBITOGRAV == "Sim") | (dados_sim_2$OBITOPUERP == "Sim, até 42 dias após o parto") | (dados_sim_2$OBITOPUERP == "Sim, de 43 dias a 1 ano")
contagem_tomt <- table(dados_sim_2$CODMUNRES[dados_sim_2$materno_consolidado == TRUE])
base$TO_MT <- as.numeric(contagem_tomt[match(base$CODMUNRES, names(contagem_tomt))])

#TO_MT_DG - total de óbitos maternos durante a gestação
dados_sim_2$gestante_sim <- dados_sim_2$OBITOGRAV == "Sim"
contagem_tomtdg <- table(dados_sim_2$CODMUNRES[dados_sim_2$gestante_sim == TRUE])
base$TO_MT_DG <- as.numeric(contagem_tomtdg[match(base$CODMUNRES, names(contagem_tomtdg))])

#TO_MT_PT - total de óbitos maternos no parto
dados_sim_2$no_parto <- dados_sim_2$MORTEPARTO == "Durante"
contagem_tomtpt <- table(dados_sim_2$CODMUNRES[dados_sim_2$no_parto == TRUE])
base$TO_MT_PT <- as.numeric(contagem_tomtpt[match(base$CODMUNRES, names(contagem_tomtpt))])

#TO_MT_AB - total de óbitos maternos no abortamento
causa_3 <- substr(dados_sim_2$CAUSABAS, 1, 3)
dados_sim_2$aborto <- causa_3 %in% c("O03", "O04", "O05", "O06", "O07", "O08")
contagem_tomtab <- table(dados_sim_2$CODMUNRES[dados_sim_2$aborto == TRUE])
base$TO_MT_AB <- as.numeric(contagem_tomtab[match(base$CODMUNRES, names(contagem_tomtab))])

#TO_MT_42 - total de óbitos maternos até 42 dias após o parto
dados_sim_2$puerperio_42 <- dados_sim_2$OBITOPUERP == "Sim, até 42 dias após o parto"
contagem_tomt42 <- table(dados_sim_2$CODMUNRES[dados_sim_2$puerperio_42 == TRUE])
base$TO_MT_42 <- as.numeric(contagem_tomt42[match(base$CODMUNRES, names(contagem_tomt42))])

#TO_MT_43 - total de óbitos maternos de 43 dias até 1 ano após o parto
dados_sim_2$puerperio_43 <- dados_sim_2$OBITOPUERP == "Sim, de 43 dias a 1 ano"
contagem_tomt43 <- table(dados_sim_2$CODMUNRES[dados_sim_2$puerperio_43 == TRUE])
base$TO_MT_43 <- as.numeric(contagem_tomt43[match(base$CODMUNRES, names(contagem_tomt43))])

#TO_MT_P - total de óbitos maternos precoces
colunas <- c("TO_MT_DG", "TO_MT_PT", "TO_MT_AB", "TO_MT_42")
base$TO_MT_P <- rowSums(base[, colunas], na.rm = TRUE)
base$TO_MT_P[base$TO_MT_P == 0] = NA

#TO_MT_P_I - total de óbitos maternos precoces de mulheres em idade fértil
is_idade_fertil <- dados_sim_2$IDADE >= 415 & dados_sim_2$IDADE <= 449
is_precoce <- (dados_sim_2$OBITOGRAV == "Sim") | 
  (dados_sim_2$OBITOPUERP == "Sim, até 42 dias após o parto") |
  (substr(dados_sim_2$CAUSABAS, 1, 3) %in% c("O03", "O04", "O05", "O06", "O07", "O08"))
dados_sim_2$precoce_idade_especifica <- is_precoce & is_idade_fertil
contagem_tomtpi <- table(dados_sim_2$CODMUNRES[dados_sim_2$precoce_idade_especifica == TRUE])
base$TO_MT_P_I <- as.numeric(contagem_tomtpi[match(base_final$CODMUNRES, names(contagem_tomtpi))])

# TO_MT_P_ES - total de óbitos maternos precoces de mulheres sem escolaridade 
is_sem_escolaridade <- dados_sim_2$ESC2010 == "Sem escolaridade"
dados_sim_2$precoce_sem_esc <- is_precoce & is_sem_escolaridade
contagem_tomtpes <- table(dados_sim_2$CODMUNRES[dados_sim_2$precoce_sem_esc == TRUE])
base$TO_MT_P_ES <- as.numeric(contagem_tomtpes[match(base$CODMUNRES, names(contagem_tomtpes))])

# TO_MT_P_EFI - total de óbitos maternos precoces de mulheres com escolaridade fundamental I 
is_fund_1 <- dados_sim_2$ESC2010 == "Fundamental I (1ª a 4ª série)"
dados_sim_2$precoce_fund_1 <- is_precoce & is_fund_1
contagem_tomtpefi <- table(dados_sim_2$CODMUNRES[dados_sim_2$precoce_fund_1 == TRUE])
base$TO_MT_P_EFI <- as.numeric(contagem_tomtpefi[match(base$CODMUNRES, names(contagem_tomtpefi))])

#TO_MT_P_EFII - total de óbitos maternos precoces de mulheres com escolaridade fundamental II
is_fund_2 <- dados_sim_2$ESC == "Fundamental II (5ª a 8ª série)"
dados_sim_2$precoce_fund_2 <- is_precoce & is_fund_2
contagem_tomtpefii <- table(dados_sim_2$CODMUNRES[dados_sim_2$precoce_fund_2 == TRUE])
base$TO_MT_P_EFII <- as.numeric(contagem_tomtpefii[match(base$CODMUNRES, names(contagem_tomtpefii))])

#TO_MT_P_EM - total de óbitos maternos precoces de mulheres com escolaridade de ensino médio
is_medio <- dados_sim_2$ESC == "Médio (antigo 2º Grau)"
dados_sim_2$precoce_medio <- is_precoce & is_medio
contagem_tomtpem <- table(dados_sim_2$CODMUNRES[dados_sim_2$precoce_medio == TRUE])
base$TO_MT_P_EM <- as.numeric(contagem_tomtpem[match(base$CODMUNRES, names(contagem_tomtpem))])

#TO_MT_P_ESI - total de óbitos maternos precoces de mulheres com escolaridade superior incompleto
is_sup_inc <- dados_sim_2$ESC == "Superior Incompleto"
dados_sim_2$precoce_sup_inc <- is_precoce & is_sup_inc
contagem_tomtpesi <- table(dados_sim_2$CODMUNRES[dados_sim_2$precoce_sup_inc == TRUE])
base$TO_MT_P_ESI <- as.numeric(contagem_tomtpesi[match(base$CODMUNRES, names(contagem_tomtpesi))])

#TO_MT_P_ESC total de óbitos maternos precoces de mulheres com escolaridade superior completo
is_sup_comp <- dados_sim_2$ESC == "Superior completo"
dados_sim_2$precoce_sup_comp <- is_precoce & is_sup_comp
contagem_tomtpesc <- table(dados_sim_2$CODMUNRES[dados_sim_2$precoce_sup_comp == TRUE])
base$TO_MT_P_ESC <- as.numeric(contagem_tomtpesc[match(base$CODMUNRES, names(contagem_tomtpesc))])

#Criando linha do estado
colunas_numericas <- sapply(base, is.numeric)
linha_estado <- as.data.frame(t(colSums(base[, colunas_numericas], na.rm = TRUE)))
linha_estado$NIVEL <- "UF"
linha_estado$CODMUNRES <- 13
linha_estado$ANO <- 2015
linha_estado$TORC[linha_estado$TORC == 0] = NA
linha_estado$TORCR[linha_estado$TORCR == 0] = NA
linha_estado$TO_MT_PT[linha_estado$TO_MT_PT == 0] = NA
linha_estado$TO_MT_P_ESI[linha_estado$TO_MT_P_ESI == 0] = NA
linha_estado$TO_FT[linha_estado$TO_FT == 0] = NA
base$NIVEL <- "MUNICIPIO"
base_completa <- rbind(linha_estado, base)
nomes <- names(base_completa)
outros_nomes <- nomes[!nomes %in% c("CODMUNRES", "NIVEL", "ANO")]
base_completa <- base_completa[, c("ANO", "NIVEL", "CODMUNRES", outros_nomes)]

# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv

write.csv(base_completa, "SIM_AM.csv")

# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main


#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

