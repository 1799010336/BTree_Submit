
//

// Created by 郑文鑫 on 2019-03-09.

//



#include "utility.hpp"

#include <functional>

#include <cstddef>

#include "exception.hpp"

#include <cstdio>
namespace sjtu {

    constexpr char address[128]="bptree_data.sjtu";//bpt地址
    template <class Key, class Value, class Compare = std::less<Key> >

    class BTree {

public:

        constexpr static long int blocksize=4096;
        struct blockhead{
        bool type;//是否为叶节点
        long int _size;//大小
        long int pos;//文件中的相对位置
        long int parent;//父节点
        long int prev;//前驱节点
        long int next;//后继节点
        blockhead():type(false),_size(0),pos(0),parent(0),prev(0),next(0){}
        };
        struct _index
        {
            long int child=0;//儿子节点
            Key key;//键值
        };
        constexpr static long int keysize=sizeof(Key);//键的大小
        constexpr static long int blockheadsize=sizeof(blockhead);//块头大小
        constexpr static long int valuesize=sizeof(Value);//value大小
        constexpr static long int blockkeynum=(blocksize-blockheadsize)/sizeof(_index)-1;//非叶节点孩子最大个数M
        constexpr static long int blockpairnum=(blocksize-blockheadsize)/(keysize+valuesize)-1;//叶子节点记录最大个数L

        struct filehead
        {
            long int blocknum;//块的个数
            long int rootpos;//根的位置
            long int headblock;//头块
            long int rearblock;//尾块
            long int _size;//大小
            filehead():blocknum(1),rootpos(0),headblock(0),rearblock(0),_size(0){}

        }info;
        struct noleafnode
        {
            _index idx[blockkeynum];
        };
        struct leafnode
        {
            pair<Key,Value> val[blockpairnum];
        };
        FILE* fp;
        //读入内存
        template <class T>
        inline void readmem(T buffer,long int buffersize,long int pos)
        {
            fseek(fp,pos*buffersize,SEEK_SET);//文件流、偏移量、起始位置
            fread(buffer,buffersize,1,fp);//读取对象，大小，数量，文件流

        }
        //读取内存
        template <class T>
        inline void writemem(T buffer,long int buffersize,long int pos)
        {
            fseek(fp,pos*buffersize,SEEK_SET);
            fwrite(buffer,buffersize,1,fp);
            fflush(fp);
        }
        //写入bpt基本数据
        inline void writeinfo()
        {
            fseek(fp,0,SEEK_SET);
            char buffer[blocksize]={0};
            memcpy(buffer,&info,sizeof(info));
            writemem(buffer,blocksize,0);
        }
        //写入节点
        template <class T>
        inline void writeblock(blockhead *head,T *data,long int pos)
        {
            char buffer[blocksize]={0};
            memcpy(buffer,head,blockheadsize);
            memcpy(buffer+blockheadsize,data,sizeof(T));
            writemem(buffer,blocksize,pos);
        }
        //读取节点
        template <class T>
        void readblock(blockhead *head,T *data,long int pos)
        {
            char buffer[blocksize]={0};
            readmem(buffer,blocksize,pos);
            memcpy(head,buffer,blockheadsize);
            memcpy(data,buffer+blockheadsize,sizeof(T));
        }
        long int memallocate()
        {
            ++info.blocknum;
            writeinfo();
            char buffer[blocksize]={0};
            writemem(buffer,blocksize,info.blocknum-1);
            return info.blocknum-1;
        }
        long int addnoleafnode(long int pa)
        {
            long int position=memallocate();
            blockhead tmp;
            noleafnode node;
            tmp.type=false;
            tmp.parent=pa;
            tmp.pos=position;
            tmp._size=0;
            writeblock(&tmp,&node,position);
            return position;
        }
        long int addleafnode(long int pa,long int next,long int prev)
        {
            long int position=memallocate();
            blockhead tmp;
            leafnode node;
            tmp.type=true;
            tmp.parent=pa;
            tmp.pos=position;
            tmp._size=0;
            tmp.next=next;
            tmp.prev=prev;
            writeblock(&tmp,&node,position);
            return position;
        }
        //插入新索引
        void insertidx(blockhead& parenthead,noleafnode& parent,long int old,long int newpos,Key& newidx)
        {
            ++parenthead._size;
            long int p=parenthead._size-2;
            while (parent.idx[p].child!=old)
            {
                parent.idx[p+1]=parent.idx[p];
                --p;
            }
            parent.idx[p+1].key=parent.idx[p].key;
            parent.idx[p].key=newidx;
            parent.idx[p+1].child=newpos;
        }
        //检查父亲是否需要分裂
        bool checkparent(blockhead& childhead)
        {
            blockhead originhead;
            noleafnode origin;
            long int originpos=childhead.parent;
            readblock(&originhead,&origin,originpos);
            if (originhead._size<blockkeynum);
                return false;
            long int parentpos;
            blockhead parenthead;
            noleafnode parent;
            //若父亲为根则创建新根
            if (originpos==info.rootpos)
            {
                long int newroot=addnoleafnode(0);
                info.rootpos=newroot;
                writeinfo();
                readblock(&parenthead,&parent,newroot);
                ++parenthead._size;
                originhead.parent=newroot;
                parent.idx[0].child=originpos;
                parentpos=newroot;
            }
            //非根则递归
            else
            {
                readblock(&parenthead,&parent,originhead.parent);
                parentpos=parenthead.pos;
            }
            if (checkparent(originhead))
            {
                parentpos=originhead.parent;
                readblock(&parenthead,&parent,parentpos);
            }
            //创建新子节点
            long int newpos=addnoleafnode(parentpos);
            blockhead newhead;
            noleafnode newnode;
            readblock(&newhead,&newnode,newpos);
            //调整数据位置
            long int midpos=originhead._size>>1;
            long int i=0,p=midpos+1;
            while (p<originhead._size)
            {
                if (origin.idx[p].child==childhead.pos)
                    childhead.parent=newpos;
                _index tmp=newnode.idx[i];
                newnode.idx[i]=origin.idx[p];
                origin.idx[p]=tmp;
                ++newhead._size;
                ++i;
                ++p;
            }
            originhead._size=midpos+1;
            insertidx(parenthead,parent,originpos,newpos,origin.idx[midpos].key);
            writeblock(&originhead,&origin,originpos);
            writeblock(&newhead,&newnode,newpos);
            writeblock(&parenthead,&parent,parentpos);
            return true;

        }
        //分裂叶子节点
        Key splitleafnode(long int pos,blockhead& originhead,leafnode& origin)
        {
            long int parentpos;
            blockhead parenthead;
            noleafnode parent;
            //是根就创建新根
            if (pos==info.rootpos)
            {
                long int rootpos=addnoleafnode(0);
                info.rootpos=rootpos;
                writeinfo();
                readblock(&parenthead,&parent,rootpos);
                ++parenthead._size;
                originhead.parent=rootpos;
                parent.idx[0].child=pos;
                parentpos=rootpos;
                parentpos=rootpos;

            }
            else
            {
                readblock(&parenthead,&parent,originhead.parent);
                parentpos=parenthead.pos;

            }
            if (checkparent(originhead))
            {
                parentpos=originhead.parent;
                readblock(&parenthead,&parent,parentpos);

            }
            long int newpos=addleafnode(parentpos,pos,originhead.next);
            //修改后继节点的前驱
            long int tmppos=originhead.next;
            blockhead tmphead;
            leafnode tmpnode;
            readblock(&tmphead,&tmpnode,tmppos);
            tmphead.prev=newpos;
            writeblock(&tmphead,&tmpnode,tmppos);
            originhead.next=newpos;
            blockhead newhead;
            leafnode newnode;
            readblock(&newhead,&newnode,newpos);
            //调整数据位置
            long int midpos=originhead._size>>1;
            long int p=midpos,i=0;
            while (p<originhead._size)
            {
                newnode.val[i].first=origin.val[p].first;
                newnode.val[i].second=origin.val[p].second;
                ++newhead._size;
                ++i;
                ++p;

            }
            originhead._size=midpos;
            insertidx(parenthead,parent,pos,newpos,origin.val[midpos].first);
            writeblock(&originhead,&origin,pos);
            writeblock(&newhead,&newnode,newpos);
            writeblock(&parenthead,&parent,parentpos);
            return newnode.val[0].first;
        }
        //创建文件
        void createfile()
        {
			if (!fp)
            {
				fp = fopen(address, "wb+");
				writeinfo();
				long int nodehead=info.blocknum;
                long int noderear=info.blocknum+1;
				info.headblock=nodehead;
				info.rearblock=noderear;
				addleafnode(0,0,noderear);
				addleafnode(0,nodehead,0);
				return;
			}
			char buffer[blocksize]={0};
			readmem(buffer,blocksize,0);
			memcpy(&info,buffer,sizeof(info));
		}


    public:

        typedef pair<const Key, Value> value_type;





        class const_iterator;

        class iterator {

       // private:

           // Your private members go here

       public:
            int nothing;
            iterator():nothing(0){}
           /* bool modify(const Value& value){



            }

            iterator() {

                // TODO Default Constructor

            }

            iterator(const iterator& other) {

                // TODO Copy Constructor

            }

            // Return a new iterator which points to the n-next elements

            iterator operator++(int) {

                // Todo iterator++

            }

            iterator& operator++() {

                // Todo ++iterator

            }

            iterator operator--(int) {

                // Todo iterator--

            }

            iterator& operator--() {

                // Todo --iterator

            }

            // Overloaded of operator '==' and '!='

            // Check whether the iterators are same

            bool operator==(const iterator& rhs) const {

                // Todo operator ==

            }

            bool operator==(const const_iterator& rhs) const {

                // Todo operator ==

            }

            bool operator!=(const iterator& rhs) const {

                // Todo operator !=

            }

            bool operator!=(const const_iterator& rhs) const {

                // Todo operator !=

            }*/

        };

        class const_iterator {

            // it should has similar member method as iterator.

            //  and it should be able to construct from an iterator.

       //private:

            // Your private members go here

        public:
            int nothing;
            const_iterator():nothing(0){}
           /* const_iterator() {

                // TODO

            }

            const_iterator(const const_iterator& other) {

                // TODO

            }

            const_iterator(const iterator& other) {

                // TODO

            }

            // And other methods in iterator, please fill by yourself.
*/
        };

        // Default Constructor and Copy Constructor

		BTree()
		{
			fp=fopen(address,"rb+");
			if (!fp)
            {
				fp=fopen(address,"wb+");
				writeinfo();
				long int nodehead=info.blocknum;
                long int noderear=info.blocknum+1;
				info.headblock=nodehead;
				info.rearblock=noderear;
				addleafnode(0,0,noderear);
				addleafnode(0,nodehead,0);
				return;
			}
			char buffer[blocksize]={0};
			readmem(buffer,blocksize,0);
			memcpy(&info,buffer,sizeof(info));
		}
		BTree(const BTree& other)
		{
			fp = fopen(address,"rb+");
			info.blocknum=other.info.blocknum;
			info.headblock=other.info.headblock;
			info.rearblock=other.info.rearblock;
			info.rootpos=other.info.rootpos;
			info._size=other.info._size;
		}
		BTree& operator=(const BTree& other)
		{
			fp = fopen(address,"rb+");
			info.blocknum=other.info.blocknum;
			info.headblock=other.info.headblock;
			info.rearblock=other.info.rearblock;
			info.rootpos=other.info.rootpos;
			info._size=other.info._size;
			return *this;
		}
		~BTree()
		{
			fclose(fp);
		}

        // Insert: Insert certain Key-Value into the database

        // Return a pair, the first of the pair is the iterator point to the new

        // element, the second of the pair is Success if it is successfully inserted

        pair<iterator, OperationResult> insert(const Key& key, const Value& value)
        {
            createfile();
			if (empty()) {
				long int rootpos=addleafnode(0,info.headblock,info.rearblock);
				blockhead tmphead;
				leafnode tmpnode;
				readblock(&tmphead,&tmpnode,info.headblock);
				tmphead.next = rootpos;
				writeblock(&tmphead,&tmpnode,info.headblock);
				readblock(&tmphead,&tmpnode,info.rearblock);
				tmphead.prev = rootpos;
				writeblock(&tmphead,&tmpnode,info.rearblock);
				readblock(&tmphead,&tmpnode,rootpos);
				++tmphead._size;
				tmpnode.val[0].first=key;
				tmpnode.val[0].second=value;
				writeblock(&tmphead,&tmpnode,rootpos);
				++info._size;
				info.rootpos=rootpos;
				writeinfo();
				pair<iterator,OperationResult> result(begin(),Success);
				return result;
			}
			//查找正确的节点位置
			char buffer[blocksize]={0};
			long int curpos=info.rootpos,curparent=0;
			while (true)
            {
				readmem(buffer,blocksize,curpos);
				blockhead temp;
				memcpy(&temp,buffer,sizeof(temp));
				//判断父亲是否更新
				if (curparent!=temp.parent)
                {
					temp.parent = curparent;
					memcpy(buffer,&temp,sizeof(temp));
					writemem(buffer,blocksize,curpos);
				}
				if (temp.type) break;
				noleafnode noleafn;
				memcpy(&noleafn,buffer+blockheadsize,sizeof(noleafn));
				long int childpos=temp._size-1;
				while (childpos>0)
				{
					if (!(noleafn.idx[childpos-1].key>key)) break;
					--childpos;
				}
				curparent=curpos;
				curpos=noleafn.idx[childpos].child;
				curpos=curpos;
			}

			blockhead info;
			memcpy(&info,buffer,sizeof(info));
			leafnode leafn;
			memcpy(&leafn,buffer+blockheadsize,sizeof(leafn));
			for (long int valuepos=0;;++valuepos)
            {
				if (valuepos<info._size&&(!(leafn.val[valuepos].first<key||leafn.val[valuepos].first>key)))
					return pair<iterator,OperationResult>(end(),Fail);

				if (valuepos >= info._size||leafn.val[valuepos].first>key)
				{
					//在此结点之前插入
					if (info._size>=blockpairnum)
					{
						long int curkey=splitleafnode(curpos,info,leafn);
						if (key>curkey)
                        {
							curpos=info.next;
							valuepos-=info._size;
							readblock(&info,&leafn,curpos);
						}
					}

					for (long int p=info._size-1;p>=valuepos;--p)
                    {
						leafn.val[p+1].first=leafn.val[p].first;
						leafn.val[p+1].second=leafn.val[p].second;
						if (p==valuepos) break;
					}
					leafn.val[valuepos].first=key;
					leafn.val[valuepos].second=value;
					++info._size;
					writeblock(&info,&leafn,curpos);
					iterator ans;
					//修改树的基本参数
					++info._size;
					writeinfo();
					pair<iterator,OperationResult> re(ans,Success);
					return re;
				}
			}
			return pair<iterator,OperationResult>(end(),Fail);
        }

        // Erase: Erase the Key-Value

        // Return Success if it is successfully erased

        // Return Fail if the key doesn't exist in the database

        OperationResult erase(const Key& key)
        {

            // TODO erase function
            createfile();
            return Fail;  // If you can't finish erase part, just remaining here.

        }

        // Return a iterator to the beginning

        iterator begin() {createfile();iterator no;return no;}

        const_iterator cbegin() const {const_iterator no;return no;}

        // Return a iterator to the end(the next element after the last)

        iterator end() {createfile();iterator no;return no;}

        const_iterator cend() const {const_iterator no;return no;}

        // Check whether this BTree is empty

        bool empty() const
        {
            if (!fp) return true;
            return info._size==0;
        }

        // Return the number of <K,V> pairs

        size_t size() const
        {
            if (!fp) return 0;
            return info._size;
        }

        // Clear the BTree

        void clear()
        {
            if (!fp) return;
            remove(address);
			filehead newfilehead;
			info=newfilehead;
			fp=nullptr;
        }

        // Return the value refer to the Key(key)

        Value at(const Key& key)
        {
            if (empty()) throw container_is_empty();
            char buffer[blocksize]={0};
            long int pos=info.rootpos,parent=0;
            while (true)
            {
                readmem(buffer,blocksize,pos);
                blockhead tmp;
                memcpy(&tmp,buffer,sizeof(tmp));
                if (parent!=tmp.parent)
                {
                    tmp.parent=parent;
                    memcpy(buffer,&tmp,sizeof(tmp));
                    writemem(buffer,blocksize,pos);
                }
                if (tmp.type) break;
                noleafnode node;
                memcpy(&node,buffer+blockheadsize,sizeof(node));
                long int childpos=tmp._size-1;
                while (childpos>0)
                {
                    if (node.idx[childpos-1].key<=key)
                        break;
                    --childpos;
                }
                pos=node.idx[childpos].child;
            }
            blockhead head;
            memcpy(&head,buffer,sizeof(head));
            leafnode lnode;
            memcpy(&lnode,buffer+blockheadsize,sizeof(lnode));
            long int valuepos=0;
            while (true)
            {
                if (valuepos<head._size&&(lnode.val[valuepos].first>=key&&lnode.val[valuepos].first<=key))
                   return lnode.val[valuepos].second;
                if (valuepos>=head._size||lnode.val[valuepos].first>key)
                    throw index_out_of_bound();
                ++valuepos;
            }
        }

        /**

         * Returns the number of elements with key

         *   that compares equivalent to the specified argument,

         * The default method of check the equivalence is !(a < b || b > a)

         */

        size_t count(const Key& key) const
        {
            if (find(key)==cend()) return 0;
            else return 1;
        }

        /**

         * Finds an element with key equivalent to key.

         * key value of the element to search for.

         * Iterator to an element with key equivalent to key.

         *   If no such element is found, past-the-end (see end()) iterator is

         * returned.

         */

        iterator find(const Key& key) {return end();}

        const_iterator find(const Key& key) const {return cend();}

    };

}  // namespace sjtu
