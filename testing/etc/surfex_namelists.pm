%pgd=(
 NAM_COVER=>{
    YCOVER              => '"ECOCLIMAP_II_EUROP_V2.2",',
    YCOVERFILETYPE      => '"DIRECT",',
 },
 NAM_ZS=>{
    YZS                 => '"gtopo30",',
    YZSFILETYPE         => '"DIRECT",',
 },
 NAM_PGD_SCHEMES=>{
    CNATURE             => '"ISBA",',
    CSEA                => '"SEAFLX",',
    CWATER              => '"WATFLX",',
    CTOWN               => '"TEB",',
 },
 NAM_SEABATHY=>{
    XUNIF_SEABATHY      => '0.,',
 },
 NAM_ISBA=>{
    YCLAY               => '"clay_fao",',
    YCLAYFILETYPE       => '"DIRECT",',
    YSAND               => '"sand_fao",',
    YSANDFILETYPE       => '"DIRECT",',
    NPATCH              => '2',
 },
);

%prep=(
 NAM_PREP_SURF_ATM=>{
  NYEAR  => '2017,',
  NMONTH => '9,',
  NDAY   => '1,',
  XTIME  => '0,',
 },
 NAM_PREP_ISBA=>{
  XHUG_SURF => '0.2,',
  XHUG_ROOT => '0.2,',
  XHUG_DEEP => '0.2,',
  XTG_SURF  => '285.,',
  XTG_ROOT  => '288.,',
  XTG_DEEP  => '292.,',
 },
 NAM_PREP_TEB=>{
  XTI_ROAD => '285.,',
  XTI_BLD  => '285.,',
  XTS_ROAD => '285.,',
  XTS_ROOF => '285.,',
  XTS_WALL => '285.,',
  XWS_ROAD => '0.,',
  XWS_ROOF => '0.,',
 },
 NAM_PREP_SEAFLUX=>{
  XSST_UNIF => '285.,',
 },
 NAM_PREP_WATFLUX=>{
  XTS_WATER_UNIF => '285.,',
 },
);

# Default values
my $CSURF_FILETYPE="ASCII";
if (defined($ENV{CSURF_FILETYPE}) && $ENV{CSURF_FILETYPE} ne "" ) { $CSURF_FILETYPE=$ENV{CSURF_FILETYPE};}
my $CTIMESERIES_FILETYPE="TEXTE";
if (defined($ENV{CTIMESERIES_FILETYPE}) && $ENV{CTIMESERIES_FILETYPE} ne "" ) { $CTIMESERIES_FILETYPE=$ENV{CTIMESERIES_FILETYPE};}
my $CFORCING_FILETYPE="NETCDF";
if (defined($ENV{CFORCING_FILETYPE}) && $ENV{CFORCING_FILETYPE} ne "" ) { $CFORCING_FILETYPE=$ENV{CFORCING_FILETYPE};}

%offline=(
 NAM_IO_OFFLINE=>{
    CSURF_FILETYPE       => '"'.$CSURF_FILETYPE.'",',
    CTIMESERIES_FILETYPE => '"'.$CTIMESERIES_FILETYPE.'",',
    CFORCING_FILETYPE    => '"'.$CFORCING_FILETYPE.'",',
    CPGDFILE             => '"PGD",',
    CPREPFILE            => '"PREP",',
    LPRINT               => '.TRUE.,',
    XTSTEP_SURF          => '300.,',
    XTSTEP_OUTPUT        => '3600.,',
    LRESTART             => '.T.,',
 },
);
