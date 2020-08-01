
SELECT
a.CensusYear,
a.StaffMatchingReference,
LAEstab,
b.QTStatus,
ContractAgreementType,
Post,
Leadership,
TOTFTE,
FT_PT,
BasePay,
BasePay_FTE,
GrossPay,
GrossPay_FTE
from SWFC_Project.SWFC.vw_AggregatedView a
left join SWFC.Workforce b
on
a.CensusYear = b.CensusYear
and
a.StaffMatchingReference = b.StaffMatchingReference


