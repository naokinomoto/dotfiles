import * as React from 'react'

interface Props extends React.Props<any> {}

interface HandlerProps extends React.Props<any> {}

const Component: React.SFC<Props & HandlerProps> = ({}) => {
    return <div />
}

export default Component
