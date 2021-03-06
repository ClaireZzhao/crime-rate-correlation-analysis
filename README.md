# crime-rate-correlation-analysis
## 서울시 자치구별 범죄 발생 수에 영향을 미치는 요인들에 대한 연구 및 분석

### 연구배경
서울시에서 10만명당 범죄 발생 건수만 평균 691.5건이라고 한다. 하지만 자치구에 따라 범죄 발생 수가 차이나는 것을 볼 수 있다. 그렇다면, 서울시에서 범죄 발생 수가 많은 자치구들이 가지고 있는 공통적인 특징이 있을까?

### 가설설정
- 인구밀도가 높을 수록 범죄 발생 수가 많다.
- 소득수준이 낮을 수록 범죄 발생 수가 많다.
- 실업률이 높을 수록 범죄 발생 수가 많다.
- CCTV설치 수가 적을 수록 범죄 발생 수가 많다.

### 데이터 수집
- 서울시 자치구별 인구밀도 데이터셋 (2021년)
- 서울시 자치구별 소득 데이터셋 (2020년, 2021년 데이터 부재 / 추후 공개예정)
- 서울시 자치구별 실업률 데이터셋 (2021년)
- 서울시 자치구별 CCTV설치 수 데이터셋 (2021년)
- 서울시 자치구별 범죄 발생 수 데이터셋 (2020년, 2021년 데이터 부재 / 추후 공개예정)

### [데이터 전처리](https://github.com/ClaireZzhao/crime-rate-correlation-analysis/blob/2239d34e5ddcdc65ef8204af48ac5377852cbd33/source-code.R#L41)

### [데이터 분석](https://github.com/ClaireZzhao/crime-rate-correlation-analysis/blob/a4500d9f42b14128da85ddbc24f6c00f9c94ab66/source-code.R#L116)

### 결론
- 자치구별 인구밀도는 범죄 발생수와 상관성이 거의 없는 것으로 보인다.
- 자치구별 소득은 높은 지역일 수록 범죄 발생 수가 많은 경향이 보인다.
- 실업률은 범죄 발생 수와 낮은 상관관계로 보이며, 범죄 발생 수에 큰 영향을 미치지 않는다. 
- CCTV설치 수에 따라 범죄 발생 수가 증가하는 경향이 보인다.

### 회고 및 고찰
이번 데이터 분석을 통해서 우리의 선입견과 고정관념이 현실과 다르다는 사실을 깨달을 수 있었습니다.
하지만, 데이터 자체가 한정적이여서 정확한 결론이라고 보기에는 힘들다고 생각합니다.
만약, 서울시 뿐만 아니라 다른 시도의 데이터셋까지 확보해서 분석한다면 더 정확한 분석결과가 나오지 않을까 생각합니다.


