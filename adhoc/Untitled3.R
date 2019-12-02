adr <- ...local_data$analysis_data_raw$data

trial_data <- module_tools$get_meta('trials')

group_data = lapply(...input$input_groups, function(g){
  g$trial_number = trial_data$Trial[ trial_data$Condition %in% unlist(g$group_conditions) ]
  g
})

adr$Group = '__no_group__'
for(g in group_data) {
  adr$Group[adr$Trial %in% g$trial_number] = g$group_name
}

sub_adr = subset(adr, Time %within% c(0,1) & Group != '__no_group__')

agg_adr = aggregate(Power ~ Time + Electrode + Subject + Group, data=sub_adr, FUN=mean) %>% dplyr::arrange(Group, Time)
agg_adr$subjel = agg_adr %$% {paste(Subject, Electrode, sep='_')}
flat_power <- agg_adr %>% split((.)$subjel) %>% sapply(getElement, 'Power')

dim(flat_power)

image(flat_power, x=0:201/100, y=1:ncol(flat_power), axes=F, ylab='')
ruta_axis(1, at=axTicks(1))
ruta_axis(2, at=1:ncol(flat_power), label=colnames(flat_power), cex.axis = 1)

