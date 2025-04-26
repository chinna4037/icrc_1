import Types "./types";
import Helper "./helper";

import Nat8 "mo:base/Nat8";
import Nat "mo:base/Nat";
import HashMap "mo:base/HashMap";
import Result "mo:base/Result";
import Time "mo:base/Time";
import Int "mo:base/Int";
import Hash "mo:base/Hash";
import Principal "mo:base/Principal";
import Blob "mo:base/Blob";
import Text "mo:base/Text";
import Array "mo:base/Array";


shared (msg) actor class ICRC1(
  _name:Text,
  _symbol : Text,
  decimalsOpt : ?Nat8,
  intialSupply : ?Nat,
)=this{
  module TransferKey {
    public func equal(a: Types.TransferKey, b: Types.TransferKey): Bool {
      a.caller == b.caller and a.args == b.args
    };

    public func hash(key: Types.TransferKey): Hash.Hash {
      let callerArray=Blob.toArray(Principal.toBlob(key.caller));
      let fromSubaccountArray=Blob.toArray(switch(key.args.fromSubaccount) {
        case(null) { Blob.fromArray([]) };
        case(?x) {x};
      });
      let toArray=Blob.toArray(Principal.toBlob(key.args.to.owner));
      let toSubaccountArray=Blob.toArray(switch(key.args.to.subaccount) {
        case(null) { Blob.fromArray([]) };
        case(?x) {x};
      });
      let amountArray=Blob.toArray(Text.encodeUtf8(Nat.toText(key.args.amount)));
      let memoArray=Blob.toArray(switch(key.args.memo) {
        case(null) { Blob.fromArray([]) };
        case(?x) {x};
      });
      let createdArray=Blob.toArray(Text.encodeUtf8(Int.toText(switch(key.args.createdAtTime){
        case (null) 0;
        case (?x) x;
      })));
      let c1=Array.append<Nat8>(callerArray,fromSubaccountArray);
      let c2=Array.append<Nat8>(c1,toArray);
      let c3=Array.append<Nat8>(c2,toSubaccountArray);
      let c4=Array.append<Nat8>(c3,amountArray);
      let c5=Array.append<Nat8>(c4,memoArray);
      let c6=Array.append<Nat8>(c5,createdArray);
      let combinedBlob=Blob.fromArray(c6);
      Blob.hash(combinedBlob);
    };

  };

  module Account{
    public func equal(a: Types.Account, b: Types.Account) : Bool {
          return a.owner == b.owner and a.subaccount == b.subaccount;
      };

      public func hash(a: Types.Account) : Nat32 {
        let ownerBlob = Blob.toArray(Principal.toBlob(a.owner));

        let subaccount = switch (a.subaccount) {
          case null Blob.fromArray([]);
          case (?sa) sa;
        };
        let subaccountBlob=Blob.toArray(subaccount);

        let combined = Array.append<Nat8>(ownerBlob,subaccountBlob);
        let combinedBlob=Blob.fromArray(combined);

        return Blob.hash(combinedBlob);
      };

  };


  let TX_WINDOW:Int=24*60*60*1_000_000_000;
  let PERMITTED_DRIFT:Int=2*60*1_000_000_000;

  let _decimals:Nat8=switch(decimalsOpt){
    case (null) 18;
    case (?x) if (x>=18) 18 else x;
  };

  var _totalSupply=0;

  let _intialSupply=switch(intialSupply){
    case null 0;
    case (?x) x;
  };

  let _mintingAccount:Types.Account=Helper.getDefaultAccount(msg.caller);

  // let defaultFee=25*Nat.pow(10,Nat8.toNat(_decimals)-2);
  let defaultFee=1;
  let multiplier=Nat.pow(10,Nat8.toNat(_decimals));

  var balances  = HashMap.HashMap<Types.Account, Nat>(0, Account.equal, Account.hash);

  balances.put(_mintingAccount,_intialSupply);
  _totalSupply+=(_intialSupply);

  let anon_account:Types.Account=Helper.getDefaultAccount(Principal.fromText("2vxsx-fae"));
  balances.put(anon_account,1_000_000);

  let dedupmap = HashMap.HashMap<Types.TransferKey, Nat>(0, TransferKey.equal, TransferKey.hash);
  var blockHeight=0;
  

  public query func name() : async Text{
    _name
  };

  public query func symbol():async Text{
    _symbol
  };

  public query func decimals():async Nat8{
    _decimals
  };

  public query func fee():async Nat{
    defaultFee
  };

  public query func totalSupply():async Nat{
    _totalSupply/multiplier
  };

  public query func mintingAccount():async ?Types.Account{
    ?_mintingAccount
  };

  public shared query (msg) func balanceOf(account:Types.Account):async Nat{
    let correctedAccount=Helper.getAccount(account);
    switch(balances.get(correctedAccount)){
      case null {
        balances.put(correctedAccount,0);
        0
      };
      case (?x) x;
    }
  };

  public shared(msg) func transfer(args:Types.TransferArgs):async Result.Result<Nat,Types.TransferError>{

    //Fee validation
    let feeToCharge= switch(args.fee) {
      case null 0;
      case (?x) {
        if (x < defaultFee) {
          return #err(#BadFee { expectedFee = defaultFee });
        };
        x
       };
    };

    //Time validation 
    switch(args.createdAtTime) {
      case(null) {};
      case(?createdAt) {
        let now=Time.now();
        if (createdAt+TX_WINDOW+PERMITTED_DRIFT < now ){
          return #err(#TooOld);
        };
        if ( createdAt > now+PERMITTED_DRIFT ){
          return #err(#CreatedInFuture {ledgerTime=now});
        }
      };
    };

    //dedepulication elimination
    let transferKey:Types.TransferKey={
      caller=msg.caller;
      args=args;
    };
    switch(dedupmap.get(transferKey)) {
      case(null) {};
      case(?x) { 
        return #err(#Duplicate {duplicateOf=x});
      };
    };

    let from:Types.Account={owner=msg.caller;subaccount=args.fromSubaccount};
    let correctdFrom=Helper.getAccount(from);
    let correctedTo=Helper.getAccount(args.to);

    //MinBurn Validation
    

    //Balance validation
    let fromBalance= await balanceOf(correctdFrom);
    let total=Nat.add(args.amount,feeToCharge);
    if ( Nat.less(fromBalance,total)){
        return #err(#InsufficientFunds {balance=fromBalance});
    };

    balances.put(correctdFrom,Nat.sub(fromBalance,total));
    let toBalance=await balanceOf(correctedTo);
    balances.put(correctedTo,Nat.add(toBalance,args.amount));

    dedupmap.put(transferKey,blockHeight);
    blockHeight+=1;

    #ok (args.amount)
  }

}