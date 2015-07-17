//    def read(self, commonfilename, directory="."):
//        '''
//name : common part of the name e.g. xxx for docword.xxx and vocab.xxx
//'''
//        self.vocabulary = file(directory+"/vocab." + commonfilename).readlines()
//        docfile = file(directory+"/docword." + commonfilename)
//        self.M=int(docfile.readline()) #doc number
//        self.V=int(docfile.readline()) #vocab size
//        self.L=int(docfile.readline()) #wordcount
//        
//        last = -1
//        maxterm_id = -1
//        minterm_id = +999999999
//        for line in docfile.readlines():
//            #print line, line.split(' ')
//            doc_id, term_id, doc_word_count = map(lambda st: int(st), line.split(' '))
//            term_id = term_id -1
//            doc_id = doc_id -1
//            
//            if len(self) < doc_id + 1:
//                doc = BagOfWordDoc()
//                self.append(doc)
//            doc = self[-1]
//            maxterm_id = max(maxterm_id, term_id)
//            minterm_id = min(minterm_id, term_id)
//            if verbose_read and last != doc_id and ismultiple(doc_id, 1000):
//                print str((doc_id *100) / self.M) + "%"
//                last = doc_id
//            doc[term_id] = doc_word_count
//        if maxterm_id - minterm_id + 1 != self.V:
//            print "warning : maxterm_id", maxterm_id, "minterm_id", minterm_id, "V size :", self.V

