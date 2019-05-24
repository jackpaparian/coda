type kind =
  | Settings
  | ChevronDown;

[@react.component]
let make = (~kind) =>
  <svg
    width="24"
    height="24"
    viewBox="0 0 24 24"
    fill="currentColor"
    xmlns="http://www.w3.org/2000/svg">
    {switch (kind) {
     | Settings =>
       <path
         fillRule="evenodd"
         clipRule="evenodd"
         d="M18.5828 9.72512L18.2759 8.98211C19.3284 6.55734 19.26 6.48468 19.054 6.27771L17.7088 4.93818L17.5754 4.82268H17.4203C17.337 4.82268 17.0936 4.82268 15.0472 5.76711L14.2938 5.45722C13.3203 3 13.2216 3 12.9323 3H11.0383C10.7534 3 10.6426 3 9.74179 5.46642L8.99194 5.77631C7.61074 5.18119 6.79944 4.87484 6.58075 4.87484H6.40017L4.95548 6.31719C4.73397 6.54255 4.65639 6.61651 5.77023 9.00392L5.46338 9.74482C3 10.7146 3 10.8083 3 11.1132V13.0027C3 13.2999 3 13.4048 5.47241 14.3077L5.77926 15.0472C4.72678 17.472 4.79662 17.5446 5.00118 17.7516L6.34641 19.0932L6.47973 19.2102H6.63704C6.71816 19.2102 6.96012 19.2102 9.00795 18.2636L9.75923 18.5756C10.7327 21.0292 10.8322 21.0292 11.1263 21.0292H13.0169C13.3075 21.0292 13.4105 21.0292 14.3155 18.5643L15.0689 18.2544C16.4501 18.8495 17.2578 19.1558 17.4758 19.1558H17.6564L19.1152 17.7008C19.3225 17.4881 19.3952 17.4142 18.2884 15.0359L18.5939 14.295C21.0585 13.3146 21.0585 13.2118 21.0585 12.916V11.0266C21.0585 10.7294 21.0585 10.6244 18.5825 9.72505L18.5828 9.72512ZM12.0295 15.168C11.1908 15.1701 10.3859 14.8391 9.79192 14.2482C9.19726 13.6573 8.86291 12.8544 8.86221 12.0171C8.8608 11.1804 9.19304 10.3768 9.78559 9.78375C10.3781 9.19145 11.1823 8.85834 12.021 8.85764C12.8597 8.85764 13.6639 9.19007 14.2572 9.78235C14.8504 10.3746 15.1827 11.1775 15.1827 12.0149C15.1813 12.8502 14.8483 13.6502 14.2579 14.2412C13.6668 14.8321 12.8661 15.1652 12.0295 15.168L12.0295 15.168Z"
       />
     | ChevronDown =>
       <path
         d="M10.8339 15.5531L5.48633 10.605C4.44743 9.64366 5.18322 8 6.65244 8L17.3476 8C18.8168 8 19.5526 9.64366 18.5137 10.605L13.1661 15.5531C12.5221 16.149 11.4779 16.149 10.8339 15.5531Z"
       />
     }}
  </svg>;