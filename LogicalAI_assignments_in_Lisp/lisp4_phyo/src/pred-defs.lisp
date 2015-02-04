(mk-pred-ttt 'PP? 
	     '(! PP-WITH PP-AT PP-ON PP-IN PP))
(mk-pred-ttt 'PPT? 
	     '(! (PP? _+)))
(mk-pred-ttt 'locative-p? 
	     '(IN (! IN ON AT BY NEAR NEARBY ABOVE BELOW OVER 
		   UNDER UP DOWN AROUND THROUGH INSIDE BETWEEN 
		   BESIDE BEYOND WITHIN BENEATH UNDERNEATH 
		   AMONG ALONG AGAINST WITH)))
(mk-pred-ttt 'nn-human? 
	     '((! NN NNP) (! HALLE TANYA SISTER)))
(mk-pred-ttt 'nn-nonhuman? 
	     '((! NN NNP) (! ~ HALLE TANYA SISTER)))