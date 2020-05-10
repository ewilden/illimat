// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import { NextApiRequest, NextApiResponse } from 'next';
import axios from 'axios';

export default async (req: NextApiRequest, res: NextApiResponse) => {
    const response = await axios.get('http://localhost:3003/move');
    const sampleGameView: GameStateView = response.data;
    res.statusCode = 200;
    res.json(sampleGameView);
}
