library("devtools")
library(readr)

# 1.) Faça a importação destes dados para o R.
head(Dados_Funcionarios)





# 2.) Considerando a variável estado civil (Est.civil):
#     a. Faça tabelas de frequência absoluta e relativa
freq_absoluta_estado_civil <- table(Dados_Funcionarios$Est.civil)
freq_relativa_estado_civil <- prop.table(freq_absoluta_estado_civil)

print(freq_absoluta_estado_civil)
print(freq_relativa_estado_civil)

#     b. Faça um gráco de barras e um de setores (pizza)
barplot(freq_absoluta_estado_civil, 
        main = "Frequência Absoluta de Estado Civil de Funcionários",
        xlab = "Estado Civil",
        ylab = "Frequência",
        col = "green",
        border = "black")
pie(freq_relativa_estado_civil,
    main = "Distribuição do Estado Civil dos Funcionários",
    col = (rainbow(length(freq_relativa_estado_civil))))





# 3.) Considerando a variável grau de instrução (Inst):
#     a. Faça tabelas de frequência absoluta e relativa
freq_absoluta_grau_instrucao <- table(Dados_Funcionarios$Inst)
freq_relativa_grau_instrucao <- prop.table(freq_absoluta_grau_instrucao)

print(freq_absoluta_grau_instrucao)
print(freq_relativa_grau_instrucao)

#     b. Faça um gráfico de barras e um de setores (pizza)
barplot(freq_absoluta_grau_instrucao,
        main = "Frequência Absoluta do Grau de Instrução de Funcionários",
        xlab = "Grau de Instrucao",
        ylab = "Frequência",
        col = "green",
        border = "black")
pie(freq_relativa_grau_instrucao,
    main = "Distribuição do Grau de instrução de Funcionários",
    col = (rainbow(length(freq_relativa_grau_instrucao))))





# 4.) Considerando a variável número de filhos (Filhos):
#     a. Faça tabelas de frequência absoluta, relativa e acumulada
freq_absoluta_num_filhos <- table(Dados_Funcionarios$Filhos)
freq_relativa_num_filhos <- prop.table(freq_absoluta_num_filhos)
freq_absoluta_acumulada_num_filhos <- cumsum(freq_absoluta_num_filhos)

print(freq_absoluta_num_filhos)
print(freq_relativa_num_filhos)
print(freq_absoluta_acumulada_num_filhos)

#     b. Faça um gráfico com as frequências relativas e outro com as 
#     frequências relativas acumuladas
barplot(freq_relativa_num_filhos,
        main = "Frequência relativa do Número de Filhos de Funcionários",
        xlab = "Número de Filhos",
        ylab = "Proporção",
        col = "green",
        border = "black")
barplot(cumsum(freq_relativa_num_filhos),
        main = "Frequência Relativa Acumulada do Número de Filhos de Funcionários",
        xlab = "Número de Filhos",
        ylab = "Proporção Acumulada do Número de Filhos",
        col = "red",
        border = "black")





# 5.) Considerando a variável salário (Salario):
#     a. Verifique os valores máximo e mínimo e qual seria o número adequado de classes
#     para a construção de uma tabela de frequência para resumir esta variável

valor_maximo_salario <- max(Dados_Funcionarios$Salario)
valor_minimo_salario <- min(Dados_Funcionarios$Salario)
print(valor_maximo_salario)
print(valor_minimo_salario)

# 1 - Regra de Sturges
total_salarios <- length(Dados_Funcionarios$Salario)
K <- round(1 + (3.3 * log10(total_salarios)))
print(paste("Número de Classes Adequado: ", K))

#     b. Faça um histograma
hist(Dados_Funcionarios$Salario,
     breaks = K,
     main = "Histograma Do Salário de Funcionários",
     xlab = "Salário",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     ylim = c(0, 20)) 
# Achei melhor colocar de 0 a 20 pq ele estava colocando até 15 e eram 16 valores numa coluna





# 6.) Considerando as variáveis estado civil (Est.civil) e grau de instrução (Inst):
#a. Faça uma tabela de contingência para resumir o cruzamento destas duas variáveis
tabela_contigencia_estadoCivil_grauInstrucao <- table(Dados_Funcionarios$Est.civil,
                                                      Dados_Funcionarios$Inst)
print(tabela_contigencia_estadoCivil_grauInstrucao)

#b. A partir da tabela de contingência, faça um gráfico de barras 
#(veja como fica as barras sobrepostas e lado a lado)

barplot(tabela_contigencia_estadoCivil_grauInstrucao,
        beside = FALSE,
        main = "Gráfico de Cruzamento do Estado Civil e Grau de Instrução Sobrepostos",
        xlab = "Grau de Instrução",
        ylab = "Frequência")
barplot(tabela_contigencia_estadoCivil_grauInstrucao,
        beside = TRUE,
        main = "Gráfico de Cruzamento do Estado Civil e Grau de Instrução Lado a Lado",
        xlab = "Grau de Instrução",
        ylab = "Frequência")





#7.) Considerando as variáveis grau de instrução (Inst) e salário (Salario): 
#a. Faça um box-plot para verificar a relação entre estas duas variáveis
boxplot(Dados_Funcionarios$Salario ~ Dados_Funcionarios$Inst,
        main = "Distribuição Salarial por Grau de Instrução",
        xlab = "Grau de Instrução",
        ylab = "Salário",
        col = "lightblue",
        notch = FALSE)

#b. Calcule a média e o desvio padrão dos salários para cada grau de instrução
media_salaria_por_grau_instrucao <- tapply(Dados_Funcionarios$Salario, Dados_Funcionarios$Inst,
                                           mean,
                                           na.rm=TRUE) 
print(media_salaria_por_grau_instrucao)

desvio_padrao_por_grau_instrucao <- tapply(Dados_Funcionarios$Salario, Dados_Funcionarios$Inst,
                                           sd,
                                           na.rm=TRUE) 
print(desvio_padrao_por_grau_instrucao)





#8.) Considerando as variáveis Anos e Meses:
#a. Calcule a idade como Anos+Meses/12(criando uma nova coluna chamada Idade)
Dados_Funcionarios$Idade <- Dados_Funcionarios$Anos+(Dados_Funcionarios$Meses/12) 
head(Dados_Funcionarios)





#9.) Considerando as variáveis Salario e Idade:
#a. Faça um gráfico para explorar a relação entre estas duas variáveis
plot(Dados_Funcionarios$Salario ~ Dados_Funcionarios$Idade,
        main = "Relação entre Salário e Idade",
        xlab = "Salário",
        ylab = "Idade")

#b. Calcule a correlação entre estas duas variáveis
correlacao <- cor(Dados_Funcionarios$Idade, Dados_Funcionarios$Salario, use = "complete.obs")
print(correlacao)