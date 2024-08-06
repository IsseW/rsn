pub enum ContainerAttr {
    Untagged(syn::Ident),
    Rename(syn::Ident, syn::Token![=], syn::Ident),
   WithMeta(syn::Ident, syn::Token![=], syn::Type),
    WithCustom(syn::Ident, syn::Token![=], syn::Type),
}

impl syn::parse::Parse for ContainerAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let tag: syn::Ident = input.parse()?;

        Ok(match tag.to_string().as_str() {
            "untagged" => ContainerAttr::Untagged(tag),
            "rename" => ContainerAttr::Rename(tag, input.parse()?, input.parse()?),
            "with_meta" => ContainerAttr::WithMeta(tag, input.parse()?, input.parse()?),
            "with_custom" => ContainerAttr::WithCustom(tag, input.parse()?, input.parse()?),

            _ => return Err(syn::Error::new(tag.span(), "Unexpected tag")),
        })
    }
}

#[derive(Default)]
pub struct ContainerAttrs {
    pub untagged: Option<syn::Ident>,
    pub rename: Option<syn::Ident>,
    pub with_meta: Option<syn::Type>,
    pub with_custom: Option<syn::Type>,
}

impl TryFrom<&Vec<syn::Attribute>> for ContainerAttrs {
    type Error = syn::Error;

    fn try_from(value: &Vec<syn::Attribute>) -> Result<Self, Self::Error> {
        let mut this = Self::default();
        for attr in value {
            if attr.path.is_ident("rsn") {
                let attrs = attr.parse_args_with(|input: syn::parse::ParseStream| {
                    input
                        .parse_terminated::<ContainerAttr, syn::Token![,]>(syn::parse::Parse::parse)
                })?;

                for attr in attrs.into_iter() {
                    match attr {
                        ContainerAttr::Untagged(tag) => {
                            if this.untagged.is_some() {
                                return Err(syn::Error::new(tag.span(), "Multiple untagged tags"));
                            }
                            this.untagged = Some(tag);
                        }
                        ContainerAttr::Rename(tag, _, rename) => {
                            if this.rename.is_some() {
                                return Err(syn::Error::new(tag.span(), "Multiple rename tags"));
                            }
                            this.rename = Some(rename);
                        }
                        ContainerAttr::WithMeta(tag, _, with_meta) => {
                            if this.with_meta.is_some() {
                                return Err(syn::Error::new(tag.span(), "Multiple with_meta tags"));
                            }
                            this.with_meta = Some(with_meta);
                        }
                        ContainerAttr::WithCustom(tag, _, with_custom) => {
                            if this.with_custom.is_some() {
                                return Err(syn::Error::new(
                                    tag.span(),
                                    "Multiple with_custom tags",
                                ));
                            }
                            this.with_custom = Some(with_custom);
                        }
                    }
                }
            }
        }
        Ok(this)
    }
}

#[derive(PartialEq, Clone)]
pub enum FieldModifier {
    Flatten,
    Default,
    Skip,
    WithExpr(syn::Token![=], syn::Expr),
}

impl FieldModifier {
    pub fn as_str(&self) -> &str {
        match self {
            FieldModifier::Flatten => "flatten",
            FieldModifier::Default => "default",
            FieldModifier::Skip => "skip",
            FieldModifier::WithExpr(_, _) => "from_meta",
        }
    }
}

impl std::fmt::Display for FieldModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[allow(dead_code)]
enum FieldAttr {
    FieldModifier(syn::Ident, FieldModifier),
    Rename(syn::Ident, syn::Token![=], syn::Ident),
    SkipBound(syn::Ident),
}

impl syn::parse::Parse for FieldAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let tag: syn::Ident = input.parse()?;

        Ok(match tag.to_string().as_str() {
            "flatten" => FieldAttr::FieldModifier(tag, FieldModifier::Flatten),
            "default" => FieldAttr::FieldModifier(tag, FieldModifier::Default),
            "skip" => FieldAttr::FieldModifier(tag, FieldModifier::Skip),
            "with_expr" => FieldAttr::FieldModifier(
                tag,
                FieldModifier::WithExpr(input.parse()?, input.parse()?),
            ),
            "rename" => FieldAttr::Rename(tag, input.parse()?, input.parse()?),
            "skip_bound" => FieldAttr::SkipBound(tag),

            _ => return Err(syn::Error::new(tag.span(), "Unexpected tag")),
        })
    }
}

#[derive(Default)]
pub struct FieldAttrs {
    pub modifier: Option<FieldModifier>,
    pub rename: Option<syn::Ident>,
    pub skip_bound: bool,
}

impl TryFrom<&Vec<syn::Attribute>> for FieldAttrs {
    type Error = syn::Error;

    fn try_from(value: &Vec<syn::Attribute>) -> Result<Self, Self::Error> {
        let mut this = Self::default();
        for attr in value {
            if attr.path.is_ident("rsn") {
                let attrs = attr.parse_args_with(|input: syn::parse::ParseStream| {
                    input.parse_terminated::<FieldAttr, syn::Token![,]>(syn::parse::Parse::parse)
                })?;

                for attr in attrs.into_iter() {
                    match attr {
                        FieldAttr::FieldModifier(tag, modifier) => {
                            if let Some(modifier) = &this.modifier {
                                return Err(syn::Error::new(tag.span(), format!("Multiple field modifier tags, this field already has a {} tag", modifier.as_str())));
                            }

                            this.modifier = Some(modifier);
                        }
                        FieldAttr::Rename(tag, _, rename) => {
                            if this.rename.is_some() {
                                return Err(syn::Error::new(tag.span(), "Multiple rename tags"));
                            }
                            this.rename = Some(rename);
                        }
                        FieldAttr::SkipBound(tag) => {
                            if this.skip_bound {
                                return Err(syn::Error::new(
                                    tag.span(),
                                    "Multiple skip bound tags",
                                ));
                            }

                            this.skip_bound = true;
                        }
                    }
                }
            }
        }
        Ok(this)
    }
}
