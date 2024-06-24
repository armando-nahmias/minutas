colunas <- c('Processo', 'Classe', 'Recorrente', 'Recorrido', 'Autuação', 'Relator')


dados <- readr::read_csv('dados/partes.csv', col_names = colunas, col_types = 'c')




