package com.us.dsb.explore.jsonapi.sketches

import io.circe.literal._
object SampleEntityCollectionResponse {

  json"""
         {
           "jsonApi": "<JsonApiObject>",
           "links": {
             "self": "/.../users?...??"
           },
           "meta": {
             "totalCount": 1,
             "filteredCount": 1,
             "entityType": {
               "typeName": "user",
               "typeSegment": "users",

               "attributesForm1": {
                 "name": {
                   "baseType": "string",
                   "typeChain": [ "string", "entityName", "userName" ],
                   "nullable": false
                 },
                 "domainName": {
                   "baseType": "string",
                   "typeChain": [ "string", "entityName", "domainName" ],
                   "nullable": false
                  }

               },
               "attributesForm2": [
                 {
                   "name": "userName",
                   "baseType": "string",
                   "...": "..."
                 },
                 {
                   "name": "domainName",
                   "...": "..."
                 },
                 "..."
               ],

               "relationshipsForm1": {
                 "domain": {
                   "meta": {
                   },
                   "links": {
                   },
                   "data": "??when populated, when not>"
                 }
               }

             }
           },
           "data": [
             {
               "type": "user",
               "id": "123",
               "attributes": {
                 "userName": "User123",
                 "domainName": "Domain1"
               },
               "relationships": {
                 "domain": {
                   "links": {
                     "related": "/.../users/123/related",
                     "self":    "/.../users/123/relationships/related"
                   },
                   "data": {
                     "type": "domain",
                     "id": "Domain1 ID"
                   },
                   "meta": "?"
                 }

               },
               "links": {
                 "self": "/.../users/123"
               }

             }
           ],
           "included": [
             {
               "type": "domain",
               "id": "Domain1 ID",
               "attributes": {
               },
               "???links": "???"


             }
           ]

         }
         """

}
