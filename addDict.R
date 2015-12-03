addDict <- function(fileName){
	source("Granice-wypowiedzi/speech.R");
	source("1_preprocessing/bandpass_butterworth.R");
	source("1_preprocessing/createFrames.R");
	source("2_extraction/mfcc.R");
	source("2_extraction/lpc.R");
	source("2_extraction/Energy.R");
	library(MASS)
	library(pracma)

	obj <- speechBorder_wav(fileName,1,1,0); #objekt typu wav
	
	low <- 0.095;
	high <- 0.22;
	sampleRate <-obj@samp.rate
	bandpass <- BandpassFiltr(obj@left,sampleRate,high,low);
	#bandpass<-obj;
	frameMatrix <-createFrames(bandpass,sampleRate);
	dim_fM <-dim(frameMatrix);
	mfccMatrix <- matrix(nrow=12,ncol=0);
	p = 12;
	lpcMatrix <- matrix(nrow=p,ncol=0);

	for(i in 1:dim_fM[1]){

		mfcc <- matrix(mfcc_calculate(as.vector(frameMatrix[i, ]),sampleRate),nrow=12,ncol=1);
		lpc <- matrix(lpc_single_vector(as.vector(frameMatrix[i, ]),p),nrow=p,ncol=1);
		mfccMatrix <- cbind(mfccMatrix,mfcc);
		lpcMatrix <- cbind(lpcMatrix,lpc);

	}

	energyMatrix<- t(Energy(frameMatrix));


	write.matrix(mfccMatrix,strcat(c('dict/mfcc',fileName,'.txt')),sep = "\t");
	write.matrix(lpcMatrix,strcat(c('dict/lpc',fileName,'.txt')),sep = "\t");
	write.matrix(energyMatrix,strcat(c('dict/ener',fileName,'.txt')),sep = "\t");


}

dictionary <- unlist(strsplit(paste(readLines("dicEntries.txt"), collapse=" "),split=" "));
for(i in 1:length(dictionary)){
	addDict(strcat(c('dzwieki/',dictionary[i],'.wav')));
}
