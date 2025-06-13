# Evaluation tests for consistancy

# 1, make sure all all KW classesspecies are 1 in the KW column
# 2, KW ecotype should be NA for all non-killer whale detections
# 3, where there is a KW ecotype, KW should be 1 and class should be KW
# 4, KW certain should be NA for all non-KW classes in classspecies
# 5, KW certain should be TRUE or FALSE for all kw classspecies or KW ==1

runTests<-function(annotationsDf, EcotypeList, ClassSpeciesList){
  
  #1- KW class for class species 1 in KW column
  if(all(annotationsDf$KW[annotationsDf$ClassSpecies == 'KW']== 1)){ 
    print('KW class checked')}else{
      
      
    print('KW detections listed as 0 in KW column, check grep statement')
    print(paste('Issue with', 
                as.character(which(annotationsDf$ClassSpecies == 'KW' & annotationsDf$KW ==0))))
    }
  
  # Check KW is 0 for non KW classes
  if(all(annotationsDf$KW[annotationsDf$ClassSpecies != 'KW']== 0)){ 
    print('KW class checked')}else{
      print('Non KW detections listed as 1 in KW column, check grep statement')
      print(paste('Issue with', 
                  as.character(which(annotationsDf$ClassSpecies != 'KW' & annotationsDf$KW ==1))))
    }
  
  
  #2, KW ecotype should be NA for all non-killer whale detections
  notKW= which(annotationsDf$KW==0)
  withEcotypes = which(!is.na(annotationsDf$Ecotype))
  value = intersect(notKW, withEcotypes)
  
  
  if(length(value>1)){
    print('Issue with ecotypes. Ecotypes assigned for non-killer whale annotations')
    print(paste('see', as.character(value)))
    
  }else{
    print('All ecotypes for on kw set to NA')
  }
  
  # 3, where there is a KW ecotype, KW should be 1 and class should be KW
  if(all(annotationsDf$KW[annotationsDf$Ecotype %in% EcotypeList]==1)){
    print('All kw annotations with ecotypes listed as KW in KW column')
  }else{
    print(paste('Issue with KW class labesl. Non ecotypes listed as 0, see',
    as.character(which(annotationsDf$KW[annotationsDf$Ecotype %in% EcotypeList]!=1))))
  }
  
  # 4, KW certain should be NA for all non-KW classes in classspecies
  if(all(is.na(annotationsDf$KW_certain[annotationsDf$ClassSpecies != 'KW']))){
    print('All KW certain set to NA for non-KW class species')
  }else{
    print('Issue with KW certain. Non NA values for not-kw')
    print(paste('see', as.character(which(
      !is.na(annotationsDf$KW_certain[annotationsDf$ClassSpecies != 'KW'])))))
  }
  
  
  # 5, KW certain should be TRUE or FALSE for all kw classspecies or KW ==1
  if(all(annotationsDf$KW_certain[annotationsDf$KW==1] %in% c(TRUE, FALSE))){
    print('All kw detections bool in KW certain')
  }else{
    print('Issue with KW_certain. Not all KW detections set to TRUE/FALSE')
  }
  
  # 5, KW certain should be TRUE or FALSE for all kw classspecies or KW ==1
  if(all(is.na(annotationsDf$KW_certain[annotationsDf$KW==0]))){
    print('All non kw detections set to na in KW certain')
  }else{
    print('Issue with KW_certain. Not all KW detections set to TRUE/FALSE')
  }
  
  # 6. Check for NA values UTC
  if(any(is.na(annotationsDf$UTC))){print('Na values found in UTC')}else{
    print('UTC checked')
  }
  
  # Check that there only classes in the class species
  classLevels = unique(annotationsDf$ClassSpecies)
  
  if(any(!classLevels %in% ClassSpeciesList)){
    problem = classLevels[(!classLevels %in% ClassSpeciesList)]
    print(paste(problem, 'found in ClassSpecies column'))
  }
  
  
  # Check that there only classes in the class species
  classLevels = unique(annotationsDf$Ecotype)
  
  if(any(!classLevels %in% c(NA, EcotypeList))){
    problem = classLevels[(!classLevels %in% c(NA, EcotypeList))]
    print(paste(problem, 'found in Ecotype column'))
  }
  
  
  #if(all(unique(annotationsDf$)))
  
  
  #7) Check that all annotations with an ecotype are tagged as KW in the KW
  # column as well as KW in the class/species
  
  values = unique(annotationsDf$ClassSpecies[!is.na(annotationsDf$Ecotype)])
  if(any(values != 'KW')){print('annotations tagged with ecotypes are assigneed to non KW class')}
  
  values = unique(annotationsDf$KW[!is.na(annotationsDf$Ecotype)])
  if(any(values != 1)){print('annotations tagged with ecotypes are assigned to KW')}
  
  
  
}