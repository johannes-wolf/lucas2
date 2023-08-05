(let [parser {}]
  (tset parser :__index parser)

  (fn parser.new [parselets tokens]
    "Instantiates a new parser object."
    (setmetatable {:i 1 :tokens tokens :parselets parselets} parser))

  (fn parser.__len [self]
    "Returns the number of tokens the parser has."
    (length self.tokens))

  (fn parser.eof-p [self]
    "Returns if the parser is at its end."
    (> self.i (length self)))

  (fn parser.lookahead [self offset]
    (let [offset (+ (or offset 0) self.i)]
      (when (<= offset (length self))
        (. self.tokens offset))))

  (fn parser.match [self kind text]
    (when (not (self:eof-p))
      (let [head (self:lookahead)]
        (and (or (not kind) (= head.kind kind))
             (or (not text) (= head.text text))))))

  (fn parser.expect [self kind text]
    (when (not (self:match kind text))
      (when (self:eof-p)
        (error (string.format "Expected %s got EOF" (or text kind))))
      (let [head (self:lookahead)]
        (error (string.format "Expected %s got %s" (or text kind)
                              (or (and head (string.format "%s (%s)" (or head.text "nil") (or head.kind))) "nil")))))
    (self:consume))

  (fn parser.consume [self]
    (let [head (self:lookahead)]
      (tset self :i (+ self.i 1))
      head))

  (fn parser.precedence [self token]
    (if token
        (let [parselet (self:find-parselet token)]
          (or (and parselet parselet.precedence) 0))
        0))

  (fn parser.find-parselet [self token]
    (if (or (= token.kind :op)
            (= token.kind :syn))
        (. self.parselets token.text)
        (. self.parselets token.kind)))

  (fn parser.parse-infix [self left precedence]
    (var left left)
    (while (< precedence (self:precedence (self:lookahead)))
      (let [token (self:consume)
            parselet (self:find-parselet token)]
        (when (not parselet)
          (error (string.format "No parselet for token '%s'" (or token.text token.kind "nil"))))
        (when (not parselet.infix)
          (error (string.format "No infix parselet for token '%s'" (or token.text token.kind "nil"))))
        (set left (parselet.infix self left token))))
    left)

  (fn parser.parse-precedence [self precedence]
    (let [token (self:consume)
          parselet (self:find-parselet token)]
        (when (not parselet)
          (error (string.format "No parselet for token '%s'" (or token.text token.kind "nil"))))
        (when (not parselet.prefix)
          (error (string.format "No prefix parselet for token '%s'" (or token.text token.kind "nil"))))
        (let [left (parselet.prefix self token)]
          (self:parse-infix left precedence))))

  (fn parser.parse [self]
    "Parser entry point function."
    (self:parse-precedence 0))

  (fn parser.parse-list [self delim stop]
    "Parse a list separated by DELIM and ended by STOP token."
    (let [list []]
      (while (not (self:match stop.kind stop.text))
        (when (> (length list) 0)
          (if (self:match delim.kind delim.text)
              (self:consume)
              (let [head (self:lookahead)]
                (error (string.format "Expected '%s' got '%s'"
                                      (or delim.text delim.kind)
                                      (or (?. head :text) (?. head :kind)))))))
        (table.insert list (self:parse)))
      (self:consume)
      list))
  
  parser)
