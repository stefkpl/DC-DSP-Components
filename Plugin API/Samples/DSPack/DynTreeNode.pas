unit DynTreeNode;

interface

uses
  ComCtrls, CommCtrl;

const
  IIL_UNCHECKED = 1;
  IIL_CHECKED   = 2;

type
  TDynTreeNode = class(TTreeNode)
  private
    FChecked : Boolean;
    procedure SetChecked(const Value: Boolean);
    function GetChecked: Boolean;
  published
    property Checked : Boolean read GetChecked write SetChecked;
  end;

implementation

procedure TDynTreeNode.SetChecked(const Value: Boolean);
var
  Item:TTvItem;
begin
  if Checked = Value then Exit;
  FChecked := Value;
  Item.hItem := ItemId;
  Item.Mask := TVIF_STATE;
  Item.StateMask := TVIS_STATEIMAGEMASK;
  if not TreeView_GetItem(Handle, Item) then Exit;
  if FChecked then Item.State := INDEXTOSTATEIMAGEMASK(IIL_CHECKED)
              else Item.State := INDEXTOSTATEIMAGEMASK(IIL_UNCHECKED);
  TreeView_SetItem(Handle,Item);
end;

function TDynTreeNode.GetChecked: Boolean;
var
  Item:TTvItem;
begin
  Result := False;
  Item.hItem := ItemId;
  Item.Mask := TVIF_STATE;
  Item.StateMask := TVIS_STATEIMAGEMASK;
  if not TreeView_GetItem(Handle, Item) then Exit;
  Result := ((integer(Item.State) and INDEXTOSTATEIMAGEMASK(IIL_CHECKED)) = INDEXTOSTATEIMAGEMASK(IIL_CHECKED));
end;

end.
 