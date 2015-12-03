recogn <- function(fileName){
	source("Granice-wypowiedzi/speech.R");
	source("1_preprocessing/bandpass_butterworth.R");
	source("1_preprocessing/createFrames.R");
	source("2_extraction/mfcc.R");
	source("2_extraction/lpc.R");
	source("2_extraction/Energy.R");
	source("3_matching/manhattan.R");

	obj <- speechBorder_wav(fileName,1,1,0); 
	
	low <- 0.095;
	high <- 0.22;
	sampleRate <-obj@samp.rate
	bandpass <- BandpassFiltr(obj@left,sampleRate,high,low);

	frameMatrix <-createFrames(bandpass,sampleRate);

	dim_fM <-dim(frameMatrix);
	mfccMatrix <- matrix(nrow=12,ncol=0);
	p = 12;
	lpcMatrix <- matrix(nrow=p,ncol=0);
	#wyliczenie cech
	for(i in 1:dim_fM[1]){
		mfcc <- matrix(mfcc_calculate(as.vector(frameMatrix[i, ]),sampleRate),nrow=12,ncol=1);
		lpc <- matrix(lpc_single_vector(as.vector(frameMatrix[i, ]),p),nrow=p,ncol=1);
		mfccMatrix <- cbind(mfccMatrix,mfcc);
		lpcMatrix <- cbind(lpcMatrix,lpc);
	}
	enerMatrix<- t(Energy(frameMatrix));

	#porownywanie z wzorcami
	minimum = 1000;
	what = 7;
	command_name = c('kawa','okno','piekarnik','pranie','swiatlo','tv','okna');
	command_id = c(1,2,3,4,5,6,2);
	names(command_id) = command_name;

	dictionary <- unlist(strsplit(paste(readLines("dicEntries.txt"), collapse=" "),split=" "));
	for(j in 1:length(dictionary)){
		#LPC
		wzorzec <- strcat(c("dict/lpc",dictionary[j],".wav.txt"));
		wzoLpcMatrix <-  as.matrix(read.table(wzorzec));
		kon1<-dim(wzoLpcMatrix)[2];
		kon2<-dim(lpcMatrix)[2];
		minkon<-min(kon1,kon2);
		sub <- abs(kon1-kon2);
		newWzoMatrix <- wzoLpcMatrix;
		newProMatrix <- lpcMatrix;
		if(kon1>kon2){
			newWzoMatrix <- newWzoMatrix[ ,(floor(sub/2)+1):(floor(sub/2)+minkon)];
		}else{
			newProMatrix <- newProMatrix[ ,(floor(sub/2)+1):(floor(sub/2)+minkon)];
		}
		lpcComVector = c(1:minkon);
		for(i in 1:minkon){
			lpcComVector[i] = manhattan(as.vector(newProMatrix[ ,i]),as.vector(newWzoMatrix[ ,i]));
		}
		
		#MFCC
		wzorzec <- strcat(c("dict/mfcc",dictionary[j],".wav.txt"));
		wzoMfccMatrix <-  as.matrix(read.table(wzorzec));
		kon1<-dim(wzoMfccMatrix)[2];
		kon2<-dim(mfccMatrix)[2];
		minkon<-min(kon1,kon2);
		sub <- abs(kon1-kon2);
		newWzoMatrix <- wzoMfccMatrix;
		newProMatrix <- mfccMatrix;
		if(kon1>kon2){
			newWzoMatrix <- newWzoMatrix[ ,(floor(sub/2)+1):(floor(sub/2)+minkon)];
		}else{
			newProMatrix <- newProMatrix[ ,(floor(sub/2)+1):(floor(sub/2)+minkon)];
		}
		mfccComVector = c(1:minkon);
		for(i in 1:minkon){
			mfccComVector[i] = manhattan(as.vector(newProMatrix[ ,i]),as.vector(newWzoMatrix[ ,i]));
		}

		#Energy
		wzorzec <- strcat(c("dict/ener",dictionary[j],".wav.txt"));
		wzoEnergyMatrix <-  as.matrix(read.table(wzorzec));
		kon1<-dim(wzoEnergyMatrix)[2];
		kon2<-dim(enerMatrix)[2];
		minkon<-min(kon1,kon2);
		sub <- abs(kon1-kon2);
		newWzoMatrix <- wzoEnergyMatrix;
		newProMatrix <- enerMatrix;
		if(kon1>kon2){
			newWzoMatrix <- newWzoMatrix[ ,(floor(sub/2)+1):(floor(sub/2)+minkon)];
		}else{
			newProMatrix <- newProMatrix[ ,(floor(sub/2)+1):(floor(sub/2)+minkon)];
		}

		enerComVector = c(1:minkon);

		for(i in 1:minkon){
			enerComVector[i] = manhattan(as.vector(newProMatrix[ ,i]),as.vector(newWzoMatrix[ ,i]));
		}

		wsp = 2;
		wsp2 = 2/10000000;
		print(c(wsp2*mean(enerComVector),wsp*mean(mfccComVector),mean(lpcComVector),wsp*mean(mfccComVector)+mean(lpcComVector)+wsp2*mean(enerComVector),dictionary[j]));
		if(wsp*mean(mfccComVector)+mean(lpcComVector)+wsp2*mean(enerComVector)<minimum){
			minimum = wsp*mean(mfccComVector)+mean(lpcComVector)+wsp2*mean(enerComVector);
			what = command_id[substring(dictionary[j],3,nchar(dictionary[j]))];
			
		}
	}
	return(what);
	
}