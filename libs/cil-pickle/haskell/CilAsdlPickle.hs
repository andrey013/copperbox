-- Sun Jan 14 23:13:05 GMT Standard Time 2007



module CilAsdlPickle where
import AsdlBase
import CilAbsSyn
 
writeCilFile :: CilFile -> CM ()
writeCilFile (x1, x2, x3, x4)
  = do pstring x1
       writeGlobals x2
       writeOptFunDec x3
       pbool x4
 
readCilFile :: CM CilFile
readCilFile
  = do x1 <- ustring
       x2 <- readGlobals
       x3 <- readOptFunDec
       x4 <- ubool
       return (x1, x2, x3, x4)
 
writeComment :: Comment -> CM ()
writeComment (x1, x2)
  = do writeLocation x1
       pstring x2
 
readComment :: CM Comment
readComment
  = do x1 <- readLocation
       x2 <- ustring
       return (x1, x2)
 
writeGlobals :: Globals -> CM ()
writeGlobals (x1) = do plist writeGlobal x1
 
readGlobals :: CM Globals
readGlobals
  = do x1 <- ulist readGlobal
       return (x1)
 
writeGlobal :: Global -> CM ()
writeGlobal (GType x1 x2)
  = do write_tag 1
       writeTypeInfo x1
       writeLocation x2
writeGlobal (GCompTag x1 x2)
  = do write_tag 2
       writeCompInfo x1
       writeLocation x2
writeGlobal (GCompTagDecl x1 x2)
  = do write_tag 3
       writeCompInfo x1
       writeLocation x2
writeGlobal (GEnumTag x1 x2)
  = do write_tag 4
       writeEnumInfo x1
       writeLocation x2
writeGlobal (GEnumTagDecl x1 x2)
  = do write_tag 5
       writeEnumInfo x1
       writeLocation x2
writeGlobal (GVarDecl x1 x2)
  = do write_tag 6
       writeVarInfo x1
       writeLocation x2
writeGlobal (GVar x1 x2 x3)
  = do write_tag 7
       writeVarInfo x1
       writeInitInfo x2
       writeLocation x3
writeGlobal (GFun x1 x2)
  = do write_tag 8
       writeFunDec x1
       writeLocation x2
writeGlobal (GAsm x1 x2)
  = do write_tag 9
       pstring x1
       writeLocation x2
writeGlobal (GPragma x1 x2)
  = do write_tag 10
       writeCilAttribute x1
       writeLocation x2
writeGlobal (GText x1)
  = do write_tag 11
       pstring x1
 
readGlobal :: CM Global
readGlobal
  = do i <- read_tag
       readGlobal' i
  where readGlobal' 1
          = do x1 <- readTypeInfo
               x2 <- readLocation
               return (GType x1 x2)
        readGlobal' 2
          = do x1 <- readCompInfo
               x2 <- readLocation
               return (GCompTag x1 x2)
        readGlobal' 3
          = do x1 <- readCompInfo
               x2 <- readLocation
               return (GCompTagDecl x1 x2)
        readGlobal' 4
          = do x1 <- readEnumInfo
               x2 <- readLocation
               return (GEnumTag x1 x2)
        readGlobal' 5
          = do x1 <- readEnumInfo
               x2 <- readLocation
               return (GEnumTagDecl x1 x2)
        readGlobal' 6
          = do x1 <- readVarInfo
               x2 <- readLocation
               return (GVarDecl x1 x2)
        readGlobal' 7
          = do x1 <- readVarInfo
               x2 <- readInitInfo
               x3 <- readLocation
               return (GVar x1 x2 x3)
        readGlobal' 8
          = do x1 <- readFunDec
               x2 <- readLocation
               return (GFun x1 x2)
        readGlobal' 9
          = do x1 <- ustring
               x2 <- readLocation
               return (GAsm x1 x2)
        readGlobal' 10
          = do x1 <- readCilAttribute
               x2 <- readLocation
               return (GPragma x1 x2)
        readGlobal' 11
          = do x1 <- ustring
               return (GText x1)
 
writeCilType :: CilType -> CM ()
writeCilType (TVoid x1)
  = do write_tag 1
       writeCilAttributes x1
writeCilType (TInt x1 x2)
  = do write_tag 2
       writeIntKind x1
       writeCilAttributes x2
writeCilType (TFloat x1 x2)
  = do write_tag 3
       writeFloatKind x1
       writeCilAttributes x2
writeCilType (TPtr x1 x2)
  = do write_tag 4
       writeCilType x1
       writeCilAttributes x2
writeCilType (TArray x1 x2 x3)
  = do write_tag 5
       writeCilType x1
       writeOptExp x2
       writeCilAttributes x3
writeCilType (TFun x1 x2 x3 x4)
  = do write_tag 6
       writeCilType x1
       writeFormalArgs x2
       pbool x3
       writeCilAttributes x4
writeCilType (TNamed x1 x2)
  = do write_tag 7
       writeTypeInfo x1
       writeCilAttributes x2
writeCilType (TComp x1 x2)
  = do write_tag 8
       writeCompInfo x1
       writeCilAttributes x2
writeCilType (TEnum x1 x2)
  = do write_tag 9
       writeEnumInfo x1
       writeCilAttributes x2
writeCilType (TBuiltinVas x1)
  = do write_tag 10
       writeCilAttributes x1
 
readCilType :: CM CilType
readCilType
  = do i <- read_tag
       readCilType' i
  where readCilType' 1
          = do x1 <- readCilAttributes
               return (TVoid x1)
        readCilType' 2
          = do x1 <- readIntKind
               x2 <- readCilAttributes
               return (TInt x1 x2)
        readCilType' 3
          = do x1 <- readFloatKind
               x2 <- readCilAttributes
               return (TFloat x1 x2)
        readCilType' 4
          = do x1 <- readCilType
               x2 <- readCilAttributes
               return (TPtr x1 x2)
        readCilType' 5
          = do x1 <- readCilType
               x2 <- readOptExp
               x3 <- readCilAttributes
               return (TArray x1 x2 x3)
        readCilType' 6
          = do x1 <- readCilType
               x2 <- readFormalArgs
               x3 <- ubool
               x4 <- readCilAttributes
               return (TFun x1 x2 x3 x4)
        readCilType' 7
          = do x1 <- readTypeInfo
               x2 <- readCilAttributes
               return (TNamed x1 x2)
        readCilType' 8
          = do x1 <- readCompInfo
               x2 <- readCilAttributes
               return (TComp x1 x2)
        readCilType' 9
          = do x1 <- readEnumInfo
               x2 <- readCilAttributes
               return (TEnum x1 x2)
        readCilType' 10
          = do x1 <- readCilAttributes
               return (TBuiltinVas x1)
        readCilType' i = tagFail i "readCilType"     
               
 
writeFormalArgs :: FormalArgs -> CM ()
writeFormalArgs (x1) = do plist writeFormalArg x1
 
readFormalArgs :: CM FormalArgs
readFormalArgs
  = do x1 <- ulist readFormalArg
       return (x1)
 
writeFormalArg :: FormalArg -> CM ()
writeFormalArg (x1, x2, x3)
  = do pstring x1
       writeCilType x2
       writeCilAttributes x3
 
readFormalArg :: CM FormalArg
readFormalArg
  = do x1 <- ustring
       x2 <- readCilType
       x3 <- readCilAttributes
       return (x1, x2, x3)
 
writeIntKind :: IntKind -> CM ()
writeIntKind (IChar) = do write_tag 1
writeIntKind (ISChar) = do write_tag 2
writeIntKind (IUChar) = do write_tag 3
writeIntKind (IInt) = do write_tag 4
writeIntKind (IUInt) = do write_tag 5
writeIntKind (IShort) = do write_tag 6
writeIntKind (IUShort) = do write_tag 7
writeIntKind (ILong) = do write_tag 8
writeIntKind (IULong) = do write_tag 9
writeIntKind (ILongLong) = do write_tag 10
writeIntKind (IULongLong) = do write_tag 11
 
readIntKind :: CM IntKind
readIntKind
  = do i <- read_tag
       readIntKind' i
  where readIntKind' 1 = do return IChar
        readIntKind' 2 = do return ISChar
        readIntKind' 3 = do return IUChar
        readIntKind' 4 = do return IInt
        readIntKind' 5 = do return IUInt
        readIntKind' 6 = do return IShort
        readIntKind' 7 = do return IUShort
        readIntKind' 8 = do return ILong
        readIntKind' 9 = do return IULong
        readIntKind' 10 = do return ILongLong
        readIntKind' 11 = do return IULongLong
 
writeFloatKind :: FloatKind -> CM ()
writeFloatKind (FFloat) = do write_tag 1
writeFloatKind (FDouble) = do write_tag 2
writeFloatKind (FLongDouble) = do write_tag 3
 
readFloatKind :: CM FloatKind
readFloatKind
  = do i <- read_tag
       readFloatKind' i
  where readFloatKind' 1 = do return FFloat
        readFloatKind' 2 = do return FDouble
        readFloatKind' 3 = do return FLongDouble
 
writeCilAttributes :: CilAttributes -> CM ()
writeCilAttributes (x1) = do plist writeCilAttribute x1
 
readCilAttributes :: CM CilAttributes
readCilAttributes
  = do x1 <- ulist readCilAttribute
       return (x1)
 
writeCilAttribute :: CilAttribute -> CM ()
writeCilAttribute (Attr x1 x2)
  = do write_tag 1
       pstring x1
       writeAttrParams x2
 
readCilAttribute :: CM CilAttribute
readCilAttribute
  = do i <- read_tag
       readCilAttribute' i
  where readCilAttribute' 1
          = do x1 <- ustring
               x2 <- readAttrParams
               return (Attr x1 x2)
 
writeAttrParams :: AttrParams -> CM ()
writeAttrParams (x1) = do plist writeAttrParam x1
 
readAttrParams :: CM AttrParams
readAttrParams
  = do x1 <- ulist readAttrParam
       return (x1)
 
writeAttrParam :: AttrParam -> CM ()
writeAttrParam (AInt x1)
  = do write_tag 1
       pint x1
writeAttrParam (AStr x1)
  = do write_tag 2
       pstring x1
writeAttrParam (ACons x1 x2)
  = do write_tag 3
       pstring x1
       writeAttrParams x2
writeAttrParam (ASizeOf x1)
  = do write_tag 4
       writeCilType x1
writeAttrParam (ASizeOfE x1)
  = do write_tag 5
       writeAttrParam x1
writeAttrParam (ASizeOfS x1)
  = do write_tag 6
       writeTypeSig x1
writeAttrParam (AAlignOf x1)
  = do write_tag 7
       writeCilType x1
writeAttrParam (AAlignOfE x1)
  = do write_tag 8
       writeAttrParam x1
writeAttrParam (AAlignOfS x1)
  = do write_tag 9
       writeTypeSig x1
writeAttrParam (AUnOp x1 x2)
  = do write_tag 10
       writeUnOp x1
       writeAttrParam x2
writeAttrParam (ABinOp x1 x2 x3)
  = do write_tag 11
       writeBinOp x1
       writeAttrParam x2
       writeAttrParam x3
writeAttrParam (ADot x1 x2)
  = do write_tag 12
       writeAttrParam x1
       pstring x2
 
readAttrParam :: CM AttrParam
readAttrParam
  = do i <- read_tag
       readAttrParam' i
  where readAttrParam' 1
          = do x1 <- uint
               return (AInt x1)
        readAttrParam' 2
          = do x1 <- ustring
               return (AStr x1)
        readAttrParam' 3
          = do x1 <- ustring
               x2 <- readAttrParams
               return (ACons x1 x2)
        readAttrParam' 4
          = do x1 <- readCilType
               return (ASizeOf x1)
        readAttrParam' 5
          = do x1 <- readAttrParam
               return (ASizeOfE x1)
        readAttrParam' 6
          = do x1 <- readTypeSig
               return (ASizeOfS x1)
        readAttrParam' 7
          = do x1 <- readCilType
               return (AAlignOf x1)
        readAttrParam' 8
          = do x1 <- readAttrParam
               return (AAlignOfE x1)
        readAttrParam' 9
          = do x1 <- readTypeSig
               return (AAlignOfS x1)
        readAttrParam' 10
          = do x1 <- readUnOp
               x2 <- readAttrParam
               return (AUnOp x1 x2)
        readAttrParam' 11
          = do x1 <- readBinOp
               x2 <- readAttrParam
               x3 <- readAttrParam
               return (ABinOp x1 x2 x3)
        readAttrParam' 12
          = do x1 <- readAttrParam
               x2 <- ustring
               return (ADot x1 x2)
 
writeCompInfo :: CompInfo -> CM ()
writeCompInfo (CompInfo x1 x2 x3 x4 x5 x6 x7)
  = do write_tag 1
       pbool x1
       pstring x2
       pint x3
       writeFieldInfos x4
       writeCilAttributes x5
       pbool x6
       pbool x7
 
readCompInfo :: CM CompInfo
readCompInfo
  = do i <- read_tag
       readCompInfo' i
  where readCompInfo' 1
          = do x1 <- ubool
               x2 <- ustring
               x3 <- uint
               x4 <- readFieldInfos
               x5 <- readCilAttributes
               x6 <- ubool
               x7 <- ubool
               return (CompInfo x1 x2 x3 x4 x5 x6 x7)
 
writeFieldInfos :: FieldInfos -> CM ()
writeFieldInfos (x1) = do plist writeFieldInfo x1
 
readFieldInfos :: CM FieldInfos
readFieldInfos
  = do x1 <- ulist readFieldInfo
       return (x1)
 
writeFieldInfo :: FieldInfo -> CM ()
writeFieldInfo (FieldInfo x1 x2 x3 x4 x5)
  = do write_tag 1
       pstring x1
       writeCilType x2
       pmaybe pint x3
       writeCilAttributes x4
       writeLocation x5
 
readFieldInfo :: CM FieldInfo
readFieldInfo
  = do i <- read_tag
       readFieldInfo' i
  where readFieldInfo' 1
          = do x1 <- ustring
               x2 <- readCilType
               x3 <- umaybe uint
               x4 <- readCilAttributes
               x5 <- readLocation
               return (FieldInfo x1 x2 x3 x4 x5)
 
writeEnumInfo :: EnumInfo -> CM ()
writeEnumInfo (EnumInfo x1 x2 x3 x4)
  = do write_tag 1
       pstring x1
       writeItems x2
       writeCilAttributes x3
       pbool x4
 
readEnumInfo :: CM EnumInfo
readEnumInfo
  = do i <- read_tag
       readEnumInfo' i
  where readEnumInfo' 1
          = do x1 <- ustring
               x2 <- readItems
               x3 <- readCilAttributes
               x4 <- ubool
               return (EnumInfo x1 x2 x3 x4)
 
writeTypeInfo :: TypeInfo -> CM ()
writeTypeInfo (TypeInfo x1 x2 x3)
  = do write_tag 1
       pstring x1
       writeCilType x2
       pbool x3
 
readTypeInfo :: CM TypeInfo
readTypeInfo
  = do i <- read_tag
       readTypeInfo' i
  where readTypeInfo' 1
          = do x1 <- ustring
               x2 <- readCilType
               x3 <- ubool
               return (TypeInfo x1 x2 x3)
 
writeVarInfos :: VarInfos -> CM ()
writeVarInfos (x1) = do plist writeVarInfo x1
 
readVarInfos :: CM VarInfos
readVarInfos
  = do x1 <- ulist readVarInfo
       return (x1)
 
writeOptVarInfo :: OptVarInfo -> CM ()
writeOptVarInfo (x1) = do pmaybe writeVarInfo x1
 
readOptVarInfo :: CM OptVarInfo
readOptVarInfo
  = do x1 <- umaybe readVarInfo
       return (x1)
 
writeVarInfo :: VarInfo -> CM ()
writeVarInfo (VarInfo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
  = do write_tag 1
       pstring x1
       writeCilType x2
       writeCilAttributes x3
       writeStorage x4
       pbool x5
       pbool x6
       writeLocation x7
       pint x8
       pbool x9
       pbool x10
 
readVarInfo :: CM VarInfo
readVarInfo
  = do i <- read_tag
       readVarInfo' i
  where readVarInfo' 1
          = do x1 <- ustring
               x2 <- readCilType               
               x3 <- readCilAttributes
               x4 <- readStorage
               x5 <- ubool
               x6 <- ubool
               x7 <- readLocation
               x8 <- uint
               x9 <- ubool
               x10 <- ubool
               return (VarInfo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
 
writeItems :: Items -> CM ()
writeItems (x1) = do plist writeItem x1
 
readItems :: CM Items
readItems
  = do x1 <- ulist readItem
       return (x1)
 
writeItem :: Item -> CM ()
writeItem (x1, x2, x3)
  = do pstring x1
       writeExp x2
       writeLocation x3
 
readItem :: CM Item
readItem
  = do x1 <- ustring
       x2 <- readExp
       x3 <- readLocation
       return (x1, x2, x3)
 
writeStorage :: Storage -> CM ()
writeStorage (NoStorage) = do write_tag 1
writeStorage (Static) = do write_tag 2
writeStorage (Register) = do write_tag 3
writeStorage (Extern) = do write_tag 4
 
readStorage :: CM Storage
readStorage
  = do i <- read_tag
       readStorage' i
  where readStorage' 1 = do return NoStorage
        readStorage' 2 = do return Static
        readStorage' 3 = do return Register
        readStorage' 4 = do return Extern
 
writeOptExp :: OptExp -> CM ()
writeOptExp (x1) = do pmaybe writeExp x1
 
readOptExp :: CM OptExp
readOptExp
  = do x1 <- umaybe readExp
       return (x1)
 
writeExps :: Exps -> CM ()
writeExps (x1) = do plist writeExp x1
 
readExps :: CM Exps
readExps
  = do x1 <- ulist readExp
       return (x1)
 
writeExp :: Exp -> CM ()
writeExp (Const x1)
  = do write_tag 1
       writeConstant x1
writeExp (Lval x1)
  = do write_tag 2
       writeLval x1
writeExp (SizeOf x1)
  = do write_tag 3
       writeCilType x1
writeExp (SizeOfE x1)
  = do write_tag 4
       writeExp x1
writeExp (SizeOfStr x1)
  = do write_tag 5
       pstring x1
writeExp (AlignOf x1)
  = do write_tag 6
       writeCilType x1
writeExp (AlignOfE x1)
  = do write_tag 7
       writeExp x1
writeExp (UnOp x1 x2 x3)
  = do write_tag 8
       writeUnOp x1
       writeExp x2
       writeCilType x3
writeExp (BinOp x1 x2 x3 x4)
  = do write_tag 9
       writeBinOp x1
       writeExp x2
       writeExp x3
       writeCilType x4
writeExp (CastE x1 x2)
  = do write_tag 10
       writeCilType x1
       writeExp x2
writeExp (AddrOf x1)
  = do write_tag 11
       writeLval x1
writeExp (StartOf x1)
  = do write_tag 12
       writeLval x1
 
readExp :: CM Exp
readExp
  = do i <- read_tag
       readExp' i
  where readExp' 1
          = do x1 <- readConstant
               return (Const x1)
        readExp' 2
          = do x1 <- readLval
               return (Lval x1)
        readExp' 3
          = do x1 <- readCilType
               return (SizeOf x1)
        readExp' 4
          = do x1 <- readExp
               return (SizeOfE x1)
        readExp' 5
          = do x1 <- ustring
               return (SizeOfStr x1)
        readExp' 6
          = do x1 <- readCilType
               return (AlignOf x1)
        readExp' 7
          = do x1 <- readExp
               return (AlignOfE x1)
        readExp' 8
          = do x1 <- readUnOp
               x2 <- readExp
               x3 <- readCilType
               return (UnOp x1 x2 x3)
        readExp' 9
          = do x1 <- readBinOp
               x2 <- readExp
               x3 <- readExp
               x4 <- readCilType
               return (BinOp x1 x2 x3 x4)
        readExp' 10
          = do x1 <- readCilType
               x2 <- readExp
               return (CastE x1 x2)
        readExp' 11
          = do x1 <- readLval
               return (AddrOf x1)
        readExp' 12
          = do x1 <- readLval
               return (StartOf x1)
 
writeConstant :: Constant -> CM ()
writeConstant (CInt64 x1 x2 x3)
  = do write_tag 1
       pint64 x1
       writeIntKind x2
       pmaybe pstring x3
writeConstant (CStr x1)
  = do write_tag 2
       pstring x1
writeConstant (CWStr x1)
  = do write_tag 3
       plist pint64 x1
writeConstant (CChr x1)
  = do write_tag 4
       pchar x1
writeConstant (CReal x1 x2 x3)
  = do write_tag 5
       pfloat x1
       writeFloatKind x2
       pmaybe pstring x3
writeConstant (CEnum x1 x2 x3)
  = do write_tag 6
       writeExp x1
       pstring x2
       writeEnumInfo x3
 
readConstant :: CM Constant
readConstant
  = do i <- read_tag
       readConstant' i
  where readConstant' 1
          = do x1 <- uint64
               x2 <- readIntKind
               x3 <- umaybe ustring
               return (CInt64 x1 x2 x3)
        readConstant' 2
          = do x1 <- ustring
               return (CStr x1)
        readConstant' 3
          = do x1 <- ulist uint64
               return (CWStr x1)
        readConstant' 4
          = do x1 <- uchar
               return (CChr x1)
        readConstant' 5
          = do x1 <- ufloat
               x2 <- readFloatKind
               x3 <- umaybe ustring
               return (CReal x1 x2 x3)
        readConstant' 6
          = do x1 <- readExp
               x2 <- ustring
               x3 <- readEnumInfo
               return (CEnum x1 x2 x3)
 
writeUnOp :: UnOp -> CM ()
writeUnOp (Neg) = do write_tag 1
writeUnOp (BNot) = do write_tag 2
writeUnOp (LNot) = do write_tag 3
 
readUnOp :: CM UnOp
readUnOp
  = do i <- read_tag
       readUnOp' i
  where readUnOp' 1 = do return Neg
        readUnOp' 2 = do return BNot
        readUnOp' 3 = do return LNot
 
writeBinOp :: BinOp -> CM ()
writeBinOp (PlusA) = do write_tag 1
writeBinOp (PlusPI) = do write_tag 2
writeBinOp (IndexPI) = do write_tag 3
writeBinOp (MinusA) = do write_tag 4
writeBinOp (MinusPI) = do write_tag 5
writeBinOp (MinusPP) = do write_tag 6
writeBinOp (Mult) = do write_tag 7
writeBinOp (Div) = do write_tag 8
writeBinOp (Mod) = do write_tag 9
writeBinOp (Shiftlt) = do write_tag 10
writeBinOp (Shiftrt) = do write_tag 11
writeBinOp (Lt) = do write_tag 12
writeBinOp (Gt) = do write_tag 13
writeBinOp (Le) = do write_tag 14
writeBinOp (Ge) = do write_tag 15
writeBinOp (Eq) = do write_tag 16
writeBinOp (Ne) = do write_tag 17
writeBinOp (BAnd) = do write_tag 18
writeBinOp (BXor) = do write_tag 19
writeBinOp (BOr) = do write_tag 20
writeBinOp (LAnd) = do write_tag 21
writeBinOp (LOr) = do write_tag 22
 
readBinOp :: CM BinOp
readBinOp
  = do i <- read_tag
       readBinOp' i
  where readBinOp' 1 = do return PlusA
        readBinOp' 2 = do return PlusPI
        readBinOp' 3 = do return IndexPI
        readBinOp' 4 = do return MinusA
        readBinOp' 5 = do return MinusPI
        readBinOp' 6 = do return MinusPP
        readBinOp' 7 = do return Mult
        readBinOp' 8 = do return Div
        readBinOp' 9 = do return Mod
        readBinOp' 10 = do return Shiftlt
        readBinOp' 11 = do return Shiftrt
        readBinOp' 12 = do return Lt
        readBinOp' 13 = do return Gt
        readBinOp' 14 = do return Le
        readBinOp' 15 = do return Ge
        readBinOp' 16 = do return Eq
        readBinOp' 17 = do return Ne
        readBinOp' 18 = do return BAnd
        readBinOp' 19 = do return BXor
        readBinOp' 20 = do return BOr
        readBinOp' 21 = do return LAnd
        readBinOp' 22 = do return LOr
 
writeOptLval :: OptLval -> CM ()
writeOptLval (x1) = do pmaybe writeLval x1
 
readOptLval :: CM OptLval
readOptLval
  = do x1 <- umaybe readLval
       return (x1)
 
writeLval :: Lval -> CM ()
writeLval (x1, x2)
  = do writeLhost x1
       writeOffset x2
 
readLval :: CM Lval
readLval
  = do x1 <- readLhost
       x2 <- readOffset
       return (x1, x2)
 
writeLhost :: Lhost -> CM ()
writeLhost (Var x1)
  = do write_tag 1
       writeVarInfo x1
writeLhost (Mem x1)
  = do write_tag 2
       writeExp x1
 
readLhost :: CM Lhost
readLhost
  = do i <- read_tag
       readLhost' i
  where readLhost' 1
          = do x1 <- readVarInfo
               return (Var x1)
        readLhost' 2
          = do x1 <- readExp
               return (Mem x1)
 
writeOffset :: Offset -> CM ()
writeOffset (NoOffset) = do write_tag 1
writeOffset (Field x1 x2)
  = do write_tag 2
       writeFieldInfo x1
       writeOffset x2
writeOffset (Index x1 x2)
  = do write_tag 3
       writeExp x1
       writeOffset x2
 
readOffset :: CM Offset
readOffset
  = do i <- read_tag
       readOffset' i
  where readOffset' 1 = do return NoOffset
        readOffset' 2
          = do x1 <- readFieldInfo
               x2 <- readOffset
               return (Field x1 x2)
        readOffset' 3
          = do x1 <- readExp
               x2 <- readOffset
               return (Index x1 x2)
 
writeOptInit :: OptInit -> CM ()
writeOptInit (x1) = do pmaybe writeInit x1
 
readOptInit :: CM OptInit
readOptInit
  = do x1 <- umaybe readInit
       return (x1)
 
writeInit :: Init -> CM ()
writeInit (SingleInit x1)
  = do write_tag 1
       writeExp x1
writeInit (CompoundInit x1 x2)
  = do write_tag 2
       writeCilType x1
       writeEltInitializers x2
 
readInit :: CM Init
readInit
  = do i <- read_tag
       readInit' i
  where readInit' 1
          = do x1 <- readExp
               return (SingleInit x1)
        readInit' 2
          = do x1 <- readCilType
               x2 <- readEltInitializers
               return (CompoundInit x1 x2)
 
writeInitInfo :: InitInfo -> CM ()
writeInitInfo (InitInfo x1)
  = do write_tag 1
       writeOptInit x1
 
readInitInfo :: CM InitInfo
readInitInfo
  = do i <- read_tag
       readInitInfo' i
  where readInitInfo' 1
          = do x1 <- readOptInit
               return (InitInfo x1)
 
writeEltInitializers :: EltInitializers -> CM ()
writeEltInitializers (x1) = do plist writeEltInitializer x1
 
readEltInitializers :: CM EltInitializers
readEltInitializers
  = do x1 <- ulist readEltInitializer
       return (x1)
 
writeEltInitializer :: EltInitializer -> CM ()
writeEltInitializer (x1, x2)
  = do writeOffset x1
       writeInit x2
 
readEltInitializer :: CM EltInitializer
readEltInitializer
  = do x1 <- readOffset
       x2 <- readInit
       return (x1, x2)
 
writeOptFunDec :: OptFunDec -> CM ()
writeOptFunDec (x1) = do pmaybe writeFunDec x1
 
readOptFunDec :: CM OptFunDec
readOptFunDec
  = do x1 <- umaybe readFunDec
       return (x1)
 
writeFunDec :: FunDec -> CM ()
writeFunDec (FunDec x1 x2 x3 x4 x5 x6 x7)
  = do write_tag 1
       writeVarInfo x1
       writeVarInfos x2
       writeVarInfos x3
       pint x4
       writeBlock x5
       pmaybe pint x6
       writeStmts x7
 
readFunDec :: CM FunDec
readFunDec
  = do i <- read_tag
       readFunDec' i
  where readFunDec' 1
          = do x1 <- readVarInfo
               x2 <- readVarInfos
               x3 <- readVarInfos
               x4 <- uint
               x5 <- readBlock
               x6 <- umaybe uint
               x7 <- readStmts
               return (FunDec x1 x2 x3 x4 x5 x6 x7)
 
writeBlock :: Block -> CM ()
writeBlock (x1, x2)
  = do writeCilAttributes x1
       writeStmts x2
 
readBlock :: CM Block
readBlock
  = do x1 <- readCilAttributes
       x2 <- readStmts
       return (x1, x2)
 
writeStmts :: Stmts -> CM ()
writeStmts (x1) = do plist writeStmt x1
 
readStmts :: CM Stmts
readStmts
  = do x1 <- ulist readStmt
       return (x1)
 
writeOptStmt :: OptStmt -> CM ()
writeOptStmt (x1) = do pmaybe writeStmt x1
 
readOptStmt :: CM OptStmt
readOptStmt
  = do x1 <- umaybe readStmt
       return (x1)
 
writeStmt :: Stmt -> CM ()
writeStmt (Stmt x1 x2 x3)
  = do write_tag 1
       writeLabels x1
       writeStmtKind x2
       pint x3
 
readStmt :: CM Stmt
readStmt
  = do i <- read_tag
       readStmt' i
  where readStmt' 1
          = do x1 <- readLabels
               x2 <- readStmtKind
               x3 <- uint
               return (Stmt x1 x2 x3)
 
writeLabels :: Labels -> CM ()
writeLabels (x1) = do plist writeLabel x1
 
readLabels :: CM Labels
readLabels
  = do x1 <- ulist readLabel
       return (x1)
 
writeLabel :: Label -> CM ()
writeLabel (Label x1 x2 x3)
  = do write_tag 1
       pstring x1
       writeLocation x2
       pbool x3
writeLabel (Case x1 x2)
  = do write_tag 2
       writeExp x1
       writeLocation x2
writeLabel (Default x1)
  = do write_tag 3
       writeLocation x1
 
readLabel :: CM Label
readLabel
  = do i <- read_tag
       readLabel' i
  where readLabel' 1
          = do x1 <- ustring
               x2 <- readLocation
               x3 <- ubool
               return (Label x1 x2 x3)
        readLabel' 2
          = do x1 <- readExp
               x2 <- readLocation
               return (Case x1 x2)
        readLabel' 3
          = do x1 <- readLocation
               return (Default x1)
 
writeStmtKind :: StmtKind -> CM ()
writeStmtKind (Instr x1)
  = do write_tag 1
       writeInstrs x1
writeStmtKind (Return x1 x2)
  = do write_tag 2
       writeOptExp x1
       writeLocation x2
writeStmtKind (Goto x1 x2)
  = do write_tag 3
       writeStmt x1
       writeLocation x2
writeStmtKind (Break x1)
  = do write_tag 4
       writeLocation x1
writeStmtKind (Continue x1)
  = do write_tag 5
       writeLocation x1
writeStmtKind (If x1 x2 x3 x4)
  = do write_tag 6
       writeExp x1
       writeBlock x2
       writeBlock x3
       writeLocation x4
writeStmtKind (Switch x1 x2 x3 x4)
  = do write_tag 7
       writeExp x1
       writeBlock x2
       writeStmts x3
       writeLocation x4
writeStmtKind (Loop x1 x2 x3 x4)
  = do write_tag 8
       writeBlock x1
       writeLocation x2
       writeOptStmt x3
       writeOptStmt x4
writeStmtKind (Block x1)
  = do write_tag 9
       writeBlock x1
writeStmtKind (TryFinally x1 x2 x3)
  = do write_tag 10
       writeBlock x1
       writeBlock x2
       writeLocation x3
writeStmtKind (TryExcept x1 x2 x3 x4)
  = do write_tag 11
       writeBlock x1
       writeExceptExp x2
       writeBlock x3
       writeLocation x4
 
readStmtKind :: CM StmtKind
readStmtKind
  = do i <- read_tag
       readStmtKind' i
  where readStmtKind' 1
          = do x1 <- readInstrs
               return (Instr x1)
        readStmtKind' 2
          = do x1 <- readOptExp
               x2 <- readLocation
               return (Return x1 x2)
        readStmtKind' 3
          = do x1 <- readStmt
               x2 <- readLocation
               return (Goto x1 x2)
        readStmtKind' 4
          = do x1 <- readLocation
               return (Break x1)
        readStmtKind' 5
          = do x1 <- readLocation
               return (Continue x1)
        readStmtKind' 6
          = do x1 <- readExp
               x2 <- readBlock
               x3 <- readBlock
               x4 <- readLocation
               return (If x1 x2 x3 x4)
        readStmtKind' 7
          = do x1 <- readExp
               x2 <- readBlock
               x3 <- readStmts
               x4 <- readLocation
               return (Switch x1 x2 x3 x4)
        readStmtKind' 8
          = do x1 <- readBlock
               x2 <- readLocation
               x3 <- readOptStmt
               x4 <- readOptStmt
               return (Loop x1 x2 x3 x4)
        readStmtKind' 9
          = do x1 <- readBlock
               return (Block x1)
        readStmtKind' 10
          = do x1 <- readBlock
               x2 <- readBlock
               x3 <- readLocation
               return (TryFinally x1 x2 x3)
        readStmtKind' 11
          = do x1 <- readBlock
               x2 <- readExceptExp
               x3 <- readBlock
               x4 <- readLocation
               return (TryExcept x1 x2 x3 x4)
 
writeExceptExp :: ExceptExp -> CM ()
writeExceptExp (x1, x2)
  = do writeInstrs x1
       writeExp x2
 
readExceptExp :: CM ExceptExp
readExceptExp
  = do x1 <- readInstrs
       x2 <- readExp
       return (x1, x2)
 
writeInstrs :: Instrs -> CM ()
writeInstrs (x1) = do plist writeInstr x1
 
readInstrs :: CM Instrs
readInstrs
  = do x1 <- ulist readInstr
       return (x1)
 
writeInstr :: Instr -> CM ()
writeInstr (Set x1 x2 x3)
  = do write_tag 1
       writeLval x1
       writeExp x2
       writeLocation x3
writeInstr (Call x1 x2 x3 x4)
  = do write_tag 2
       writeOptLval x1
       writeExp x2
       writeExps x3
       writeLocation x4
writeInstr (Asm x1 x2 x3 x4 x5 x6)
  = do write_tag 3
       writeCilAttributes x1
       plist pstring x2
       writeAsmOutputs x3
       writeAsmInputs x4
       plist pstring x5
       writeLocation x6
 
readInstr :: CM Instr
readInstr
  = do i <- read_tag
       readInstr' i
  where readInstr' 1
          = do x1 <- readLval
               x2 <- readExp
               x3 <- readLocation
               return (Set x1 x2 x3)
        readInstr' 2
          = do x1 <- readOptLval
               x2 <- readExp
               x3 <- readExps
               x4 <- readLocation
               return (Call x1 x2 x3 x4)
        readInstr' 3
          = do x1 <- readCilAttributes
               x2 <- ulist ustring
               x3 <- readAsmOutputs
               x4 <- readAsmInputs
               x5 <- ulist ustring
               x6 <- readLocation
               return (Asm x1 x2 x3 x4 x5 x6)
 
writeAsmOutputs :: AsmOutputs -> CM ()
writeAsmOutputs (x1) = do plist writeAsmOutput x1
 
readAsmOutputs :: CM AsmOutputs
readAsmOutputs
  = do x1 <- ulist readAsmOutput
       return (x1)
 
writeAsmOutput :: AsmOutput -> CM ()
writeAsmOutput (x1, x2)
  = do pstring x1
       writeLval x2
 
readAsmOutput :: CM AsmOutput
readAsmOutput
  = do x1 <- ustring
       x2 <- readLval
       return (x1, x2)
 
writeAsmInputs :: AsmInputs -> CM ()
writeAsmInputs (x1) = do plist writeAsmInput x1
 
readAsmInputs :: CM AsmInputs
readAsmInputs
  = do x1 <- ulist readAsmInput
       return (x1)
 
writeAsmInput :: AsmInput -> CM ()
writeAsmInput (x1, x2)
  = do pstring x1
       writeExp x2
 
readAsmInput :: CM AsmInput
readAsmInput
  = do x1 <- ustring
       x2 <- readExp
       return (x1, x2)
 
writeLocation :: Location -> CM ()
writeLocation (Location x1 x2 x3)
  = do write_tag 1
       pint x1
       pstring x2
       pint x3
 
readLocation :: CM Location
readLocation
  = do i <- read_tag
       readLocation' i
  where readLocation' 1
          = do x1 <- uint
               x2 <- ustring
               x3 <- uint
               return (Location x1 x2 x3)
 
writeTypeSigs :: TypeSigs -> CM ()
writeTypeSigs (x1) = do plist writeTypeSig x1
 
readTypeSigs :: CM TypeSigs
readTypeSigs
  = do x1 <- ulist readTypeSig
       return (x1)
 
writeTypeSig :: TypeSig -> CM ()
writeTypeSig (TSArray x1 x2 x3)
  = do write_tag 1
       writeTypeSig x1
       pmaybe pint64 x2
       writeCilAttributes x3
writeTypeSig (TSPtr x1 x2)
  = do write_tag 2
       writeTypeSig x1
       writeCilAttributes x2
writeTypeSig (TSComp x1 x2 x3)
  = do write_tag 3
       pbool x1
       pstring x2
       writeCilAttributes x3
writeTypeSig (TSFun x1 x2 x3 x4)
  = do write_tag 4
       writeTypeSig x1
       writeTypeSigs x2
       pbool x3
       writeCilAttributes x4
writeTypeSig (TSEnum x1 x2)
  = do write_tag 5
       pstring x1
       writeCilAttributes x2
writeTypeSig (TSBase x1)
  = do write_tag 6
       writeCilType x1
 
readTypeSig :: CM TypeSig
readTypeSig
  = do i <- read_tag
       readTypeSig' i
  where readTypeSig' 1
          = do x1 <- readTypeSig
               x2 <- umaybe uint64
               x3 <- readCilAttributes
               return (TSArray x1 x2 x3)
        readTypeSig' 2
          = do x1 <- readTypeSig
               x2 <- readCilAttributes
               return (TSPtr x1 x2)
        readTypeSig' 3
          = do x1 <- ubool
               x2 <- ustring
               x3 <- readCilAttributes
               return (TSComp x1 x2 x3)
        readTypeSig' 4
          = do x1 <- readTypeSig
               x2 <- readTypeSigs
               x3 <- ubool
               x4 <- readCilAttributes
               return (TSFun x1 x2 x3 x4)
        readTypeSig' 5
          = do x1 <- ustring
               x2 <- readCilAttributes
               return (TSEnum x1 x2)
        readTypeSig' 6
          = do x1 <- readCilType
               return (TSBase x1)



