// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import { NextApiRequest, NextApiResponse } from 'next';
import axios, { Method } from 'axios';

export default async (req: NextApiRequest, res: NextApiResponse) => {
    console.log('general path body');
    console.log(req.body);
    const {
        query: { path },
        cookies: {
            USER_ID: userIdCookie,
        },
        method: methodName,
    } = req;

    const pathAsArray = Array.isArray(path) ? path : [path];

    const response = await axios.request({
        url: `http://localhost:3003/${pathAsArray.join('/')}`,
        method: methodName as Method,
        headers: userIdCookie ? {
            Cookie: `USER_ID=${userIdCookie}`,
        } : {},
        data: req.body,
    });
    console.log('axios response');
    console.log(response);

    console.log('general path response');
    const respData = response.data;
    console.log(respData);
    if (response.headers['set-cookie']) {
        console.log('found cookie!');
        console.log(response.headers['set-cookie']);
        res.writeHead(200, {
            'Set-Cookie': response.headers['set-cookie'][0],
            'Content-Type': 'application/json',
        });
        res.end(JSON.stringify(respData));
    } else {
        res.statusCode = 200;
        res.json(respData);
    }
}
