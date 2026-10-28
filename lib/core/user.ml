type t = {
  id : User_id.t;
  oidc_issuer : string;
  oidc_subject : string;
  display_name : string;
  email : string option;
  locale : string;
  timezone : string;
  default_currency : string;
  pay_cycle : Pay_cycle_setting.t;
  created_at : Ptime.t;
}

type settings = { locale : string; timezone : string; default_currency : string; pay_cycle : Pay_cycle_setting.t }
