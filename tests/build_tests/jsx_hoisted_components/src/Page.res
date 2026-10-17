module SidebarAlias = Sidebar
module ProviderAlias = SidebarAlias.Provider

@react.component
let make = () => {
  <Sidebar.Provider>
    <Sidebar.Group.Provider>
      <ProviderAlias>
        <Sidebar.WithProps>
          <Sidebar.Memo>
            <Sidebar.ForwardRef>
              <Sidebar.WithPrivate>
                <Sidebar.Sidebar>
                  <Sidebar.External>
                    <Sidebar.ExternalManual>
                      <External.Debug>{React.string("child")}</External.Debug>
                    </Sidebar.ExternalManual>
                  </Sidebar.External>
                </Sidebar.Sidebar>
              </Sidebar.WithPrivate>
            </Sidebar.ForwardRef>
          </Sidebar.Memo>
        </Sidebar.WithProps>
      </ProviderAlias>
    </Sidebar.Group.Provider>
  </Sidebar.Provider>
}
